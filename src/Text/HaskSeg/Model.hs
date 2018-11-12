{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Model (applyModel, combinations, oneWordProb, g, distribution, sampleSite, sample) where

import Prelude hiding (lookup)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM, foldM)
import System.IO (withFile, hPutStr, IOMode(..), readFile)
import System.Random (getStdGen, mkStdGen)
import Data.List (unfoldr, nub, mapAccumL, intercalate, sort, foldl1')
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Math.Combinatorics.Exact.Binomial (choose)
import Control.Monad.Loops
import Control.Monad.Log
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Reader.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Tuple (swap)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Random
import System.Random.Shuffle (shuffleM)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified System.Console.ANSI as A
import Text.HaskSeg.Probability (Prob, LogProb, Probability(..), showDist, sampleCategorical, Categorical)
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..))
import Text.HaskSeg.Metrics (f1)
import Text.HaskSeg.Utils (readModel, writeModel, readDataset, writeDataset, readVocabulary, writeVocabulary)
import Text.HaskSeg.Location (randomFlip, createData, randomizeLocations, updateLocations, nonConflicting, wordsToSites, siteToWords, updateLocations')
import Text.HaskSeg.Lookup (cleanLookup, initializeLookups, computeUpdates)
import Text.HaskSeg.Counts (cleanCounts, initializeCounts, updateCounts, addCounts, subtractCounts)

import Data.Char (toLower, isPunctuation)
import Codec.Compression.GZip (compress, decompress)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Text.HaskSeg.Probability (Prob, LogProb, Probability(..), showDist, sampleCategorical)


likelihood :: (Probability p, Categorical p, Show p, MonadIO m, MonadRandom m, (MonadReader (Params p)) m, MonadState (SamplingState Char) m, MonadLog (WithSeverity String) m) => m p
likelihood = do
  SamplingState{..} <- get
  Params{..} <- ask
  ps <- sequence $ map (\(w, n) -> oneWordProb _counts _charProb _stop _dontStop _alpha n w) (Map.toList _counts)
  let p = foldl1' (*) ps
  return $! p


-- | Run one sampling iteration
sample :: (Probability p, Categorical p, Show p, MonadIO m, MonadRandom m, (MonadReader (Params p)) m, MonadState (SamplingState Char) m, MonadLog (WithSeverity String) m) => Int -> m ()
sample i = do
  ll <- unwrap <$> likelihood
  state <- get
  params <- ask
  logInfo (printf "\nIteration #%d" i)
  let indices = Set.fromList [i | (l, i) <- zip ((Vector.toList (_locations state))) [0..], _static l == False]
  
  iterateUntilM (\s -> Set.size s == 0) sampleSite indices
  state' <- get
  put $ state' { _counts=cleanCounts (_counts state'), _startLookup=cleanLookup (_startLookup state'), _endLookup=cleanLookup (_endLookup state') }
  ll' <- unwrap <$> likelihood
  let guesses' = Set.fromList $ Vector.toList $ Vector.findIndices (\x -> _morphFinal x && (not $ _static x)) (_locations state')
      guesses = Set.fromList $ Vector.toList $ Vector.findIndices (\x -> _morphFinal x && (not $ _static x)) (_locations state)
      score = f1 guesses (_gold params)
      score' = f1 guesses' (_gold params)
      --ll = (unwrap ll) -- / ((fromIntegral . length) guesses)
  
  logInfo (printf "Log-likelihood old/new: %.3v/%.3v\tF-Score old/new: %.3f/%.3f" ll ll' score score')
  return $! ()



applyModel :: (Probability p) => (Params p, Locations l) -> [l] -> [l]
applyModel (params, locs) xs = xs

combinations :: (MonadLog (WithSeverity String) m, Show p, Probability p) => Int -> m (Vector p)
combinations n = do
  return $ Vector.generate (n + 1) (fromDouble . fromIntegral . (n `choose`))

-- | Compute the log-probability of generating the given word n times, based on counts
oneWordProb :: (Show p, MonadLog (WithSeverity String) m, Probability p, Show elem, Ord elem) => Counts elem -> p -> p -> p -> p -> Int -> Morph elem -> m p
oneWordProb counts charProb stopProb dontStopProb alpha n word = do
  let mu = ((dontStopProb * charProb) ^ (length word)) * stopProb
      total = fromIntegral $ sum $ Map.elems counts
      count = fromIntegral $ Map.findWithDefault 0 word counts
      numer = ((alpha * mu) + count)
      denom = (alpha + total)
  --logInfo (printf "%v: mu=%v, total=%v, count=%v, numer=%v, denom=%v" word (show mu) (show total) (show count) (show numer) (show denom))
  return $! ((numer ^ n) / (denom ^ n))

-- | Compute the log-probability of setting a single set of m sites, out of n, to positive
g :: (Show p, MonadLog (WithSeverity String) m, Ord elem, Show elem, Probability p) => Counts elem -> p -> p -> p -> Morph elem -> Morph elem -> p -> Int -> Int -> m p
g counts charProb stopProb dontStopProb before after alpha n m = do
  beforeProb <- oneWordProb counts charProb stopProb dontStopProb alpha m before
  afterProb <- oneWordProb counts charProb stopProb dontStopProb alpha m after
  let posProb = beforeProb * afterProb
  negProb <- oneWordProb counts charProb stopProb dontStopProb alpha (n - m) (before Vector.++ after)
  return $! posProb * negProb

-- | Compute the log-categorical distribution of possible number of sites to set to positive:
--     P(m) = (n choose m) * g(m)
distribution :: (Show p, MonadLog (WithSeverity String) m, Probability p, Show elem, Ord elem, Show p) => Counts elem -> p -> p -> p -> Morph elem -> Morph elem -> p -> Int -> m (Vector p)
distribution counts charProb stopProb dontStopProb before after alpha n = do
  gs <- (liftM Vector.fromList . sequence) [g counts charProb stopProb dontStopProb before after alpha n m | m <- [0..n]]
  combs <- combinations n
  let unScaled = Vector.map (\(x, y) -> x * y) (Vector.zip combs gs)
  return $! unScaled

-- | Randomly sample a site from those currently available, and then block-sample all compatible sites, returning the updated list of available sites
sampleSite :: (Probability p, Categorical p, Show p, MonadIO m, MonadLog (WithSeverity String) m, MonadRandom m, MonadState (SamplingState Char) m, MonadReader (Params p) m) => Set Int -> m (Set Int)
sampleSite ix = do
  params@(Params{..}) <- ask
  state@(SamplingState{..}) <- get

  logDebug ('\n':(printf "%v" params))
  --s'' <- showFullState Nothing Nothing
  logDebug (printf "%v" params)
  i <- uniform ix
  
  (a, b) <- siteToWords i
  let c = a Vector.++ b
  (fullSites', splitSites') <- wordsToSites i _startLookup _endLookup a b
  let fullSites = Set.intersection fullSites' ix
      splitSites = Set.intersection splitSites' ix      
      sites = Set.union fullSites splitSites
      nSplit = Set.size splitSites
      nFull = Set.size fullSites
      cs' = (subtractCounts c nFull . subtractCounts a nSplit . subtractCounts b nSplit) _counts
  d <- distribution cs' _charProb _stop _dontStop a b _alpha (Set.size sites)
  numPos <- sampleCategorical d

  put state{ _counts=cleanCounts cs' }
  logDebug (printf "Pivot: %d" i)
  logDebug (printf "Morphs: left=%v, right=%v" (show a) (show b))
  logDebug (printf "Matching, non-conflicting positive sites: [%v]" splitSites)
  logDebug (printf "Matching, non-conflicting negative sites: [%v]" fullSites)  
  --s <- showFullState (Just i) (Just sites)
  --logDebug s
  logDebug (printf "Distribution: [%v]" (showDist d))
  logDebug (printf "Chose positive count: %d" numPos)

  sites' <- shuffleM (Set.toList sites)
  let (pos, neg) = splitAt numPos sites'
      pos' = Set.fromList pos
      neg' = Set.fromList neg
      nPos = length pos
      nNeg = length neg
      cs'' = (addCounts c nNeg . addCounts a nPos . addCounts b nPos) cs'
      cs''' = Map.fromList $ [(k, v) | (k, v) <- Map.toList cs'', v /= 0]
      locations' = updateLocations' (_value (_locations Vector.! i)) _locations pos' neg'
      (upS, upE) = computeUpdates splitSites fullSites a b
      luS' = Map.unionWith (Set.\\) _startLookup upS
      luE' = Map.unionWith (Set.\\) _endLookup upE      
      (upS', upE') = computeUpdates pos' neg' a b
      luS = cleanLookup $ Map.unionWith Set.union luS' upS'
      luE = cleanLookup $ Map.unionWith Set.union luE' upE'
      ix' = ix Set.\\ sites
  put $ SamplingState cs''' locations' luS luE ix'
  --s' <- showFullState Nothing Nothing
  --logDebug s'
  return $! ix Set.\\ sites

-- toLookup :: (Ord a) => [Location a] -> Map [a] [[a]]
-- toLookup ls = Map.fromList ls''
--   where
--     ls' = split' (\x -> _static x == False) ls
--     ls'' = [(map _value l, reverse $ map (map _value) (split' (\x -> _morphFinal x == False) l)) | l <- ls']    


-- split' :: (a -> Bool) -> [a] -> [[a]]
-- split' p xs = go xs []
--   where
--     go [] acc = acc
--     go xs' acc = go rest (x:acc)
--       where
--         (pref, r:rest) = span p xs'
--         x = pref ++ [r]

-- --
-- --   Sampling-distribution-related functions
-- --

-- -- | Compute the log-probability of generating the given word n times, based on counts
-- oneWordProb :: (Show p, Show elem, MonadLog (WithSeverity String) m, Probability p, Show elem, Ord elem) => Counts elem -> p -> p -> p -> p -> Int -> Morph elem -> m p
-- oneWordProb counts charProb stopProb dontStopProb alpha n word = do
--   let mu = ((dontStopProb * charProb) ^ (length word)) * stopProb
--       total = fromIntegral $ sum $ Map.elems counts
--       count = fromIntegral $ Map.findWithDefault 0 word counts
--       numer = ((alpha * mu) + count)
--       denom = (alpha + total)
--   --logInfo (printf "%v: mu=%v, total=%v, count=%v, numer=%v, denom=%v" word (show mu) (show total) (show count) (show numer) (show denom))
--   return $! ((numer ^ n) / (denom ^ n))

-- -- | Compute the log-probability of setting a single set of m sites, out of n, to positive
-- g :: (Show p, Show elem, MonadLog (WithSeverity String) m, Ord elem, Show elem, Probability p) => Counts elem -> p -> p -> p -> Morph elem -> Morph elem -> p -> Int -> Int -> m p
-- g counts charProb stopProb dontStopProb before after alpha n m = do
--   beforeProb <- oneWordProb counts charProb stopProb dontStopProb alpha m before
--   afterProb <- oneWordProb counts charProb stopProb dontStopProb alpha m after
--   let posProb = beforeProb * afterProb
--   negProb <- oneWordProb counts charProb stopProb dontStopProb alpha (n - m) (before Vector.++ after)
--   return $! posProb * negProb
  
-- -- | 
-- combinations :: (MonadLog (WithSeverity String) m, Show p, Probability p) => Int -> m (Vector p)
-- combinations n = do
--   let cs = Vector.generate (n + 1) (fromDouble . fromIntegral . (n `choose`))
--   --logDebug (printf "comb(%d) = %v" n (show cs))
--   return $! cs
--     --tot = fromIntegral $ Vector.sum cs
--     --cs' = Vector.map (\c -> fromDouble $ (fromIntegral c) / tot) cs

-- -- | Compute the log-categorical distribution of possible number of sites to set to positive:
-- --     P(m) = (n choose m) * g(m)
-- distribution :: (Show p, Show elem, MonadLog (WithSeverity String) m, Probability p, Show elem, Ord elem, Show p) => Counts elem -> p -> p -> p -> Morph elem -> Morph elem -> p -> Int -> m (Vector p)
-- distribution counts charProb stopProb dontStopProb before after alpha n = do
--   gs <- (liftM Vector.fromList . sequence) [g counts charProb stopProb dontStopProb before after alpha n m | m <- [0..n]]
--   combs <- combinations n
--   let unScaled = Vector.map (\(x, y) -> x * y) (Vector.zip combs gs)
--   return $! unScaled

-- --
-- --   Sampling-mechanism-related functions
-- --

-- -- | Randomly sample a site from those currently available, and then block-sample all compatible sites, returning the updated list of available sites
-- sampleSite :: (Categorical p, Show elem, Show p, Probability p, MonadIO m, MonadLog (WithSeverity String) m, MonadRandom m, MonadState (SamplingState elem) m, MonadReader (Params p) m) => Set Int -> m (Set Int)
-- sampleSite ix = do
--   params@(Params{..}) <- ask
--   state@(SamplingState{..}) <- get

--   logDebug ('\n':(printf "%v" params))
--   --s'' <- showFullState Nothing Nothing
--   logDebug (printf "%v" params)
--   i <- uniform ix
  
--   (a, b) <- siteToWords i
--   let c = a Vector.++ b
--   (fullSites', splitSites') <- wordsToSites i _startLookup _endLookup a b
--   let fullSites = Set.intersection fullSites' ix
--       splitSites = Set.intersection splitSites' ix      
--       sites = Set.union fullSites splitSites
--       nSplit = Set.size splitSites
--       nFull = Set.size fullSites
--       cs' = (subtractCounts c nFull . subtractCounts a nSplit . subtractCounts b nSplit) _counts
--   d <- distribution cs' _charProb _stop _dontStop a b _alpha (Set.size sites)
--   numPos <- sampleCategorical d

--   put state{ _counts=cleanCounts cs' }
--   logDebug (printf "Pivot: %d" i)
--   logDebug (printf "Morphs: left=%v, right=%v" (show a) (show b))
--   logDebug (printf "Matching, non-conflicting positive sites: [%v]" splitSites)
--   logDebug (printf "Matching, non-conflicting negative sites: [%v]" fullSites)  
--   --s <- showFullState (Just i) (Just sites)
--   --logDebug s
--   logDebug (printf "Distribution: [%v]" (showDist d))
--   logDebug (printf "Chose positive count: %d" numPos)

--   sites' <- shuffleM (Set.toList sites)
--   let (pos, neg) = splitAt numPos sites'
--       pos' = Set.fromList pos
--       neg' = Set.fromList neg
--       nPos = length pos
--       nNeg = length neg
--       cs'' = (addCounts c nNeg . addCounts a nPos . addCounts b nPos) cs'
--       cs''' = Map.fromList $ [(k, v) | (k, v) <- Map.toList cs'', v /= 0]
--       locations' = updateLocations' (_value (_locations Vector.! i)) _locations pos' neg'
--       (upS, upE) = computeUpdates splitSites fullSites a b
--       luS' = Map.unionWith (Set.\\) _startLookup upS
--       luE' = Map.unionWith (Set.\\) _endLookup upE      
--       (upS', upE') = computeUpdates pos' neg' a b
--       luS = cleanLookup $ Map.unionWith Set.union luS' upS'
--       luE = cleanLookup $ Map.unionWith Set.union luE' upE'
--       ix' = ix Set.\\ sites
--   put $ SamplingState cs''' locations' luS luE ix'
--   --s' <- showFullState Nothing Nothing
--   --logDebug s'
--   return $! ix Set.\\ sites

-- -- | Run one sampling iteration
-- sample :: (Show p, Show elem, Categorical p, Probability p, MonadIO m, MonadRandom m, (MonadReader (Params p)) m, MonadState (SamplingState elem) m, MonadLog (WithSeverity String) m) => Int -> m ()
-- sample i = do
--   ll <- unwrap <$> likelihood
--   state <- get
--   params <- ask
--   logInfo (printf "\nIteration #%d" i)
--   let indices = Set.fromList [i | (l, i) <- zip ((Vector.toList (_locations state))) [0..], _static l == False]
  
--   iterateUntilM (\s -> Set.size s == 0) sampleSite indices
--   state' <- get
--   put $ state' { _counts=cleanCounts (_counts state'), _startLookup=cleanLookup (_startLookup state'), _endLookup=cleanLookup (_endLookup state') }
--   ll' <- unwrap <$> likelihood
--   let guesses' = Set.fromList $ Vector.toList $ Vector.findIndices (\x -> _morphFinal x && (not $ _static x)) (_locations state')
--       guesses = Set.fromList $ Vector.toList $ Vector.findIndices (\x -> _morphFinal x && (not $ _static x)) (_locations state)
--       score = f1 guesses (_gold params)
--       score' = f1 guesses' (_gold params)
--       --ll = (unwrap ll) -- / ((fromIntegral . length) guesses)
  
--   logInfo (printf "Log-likelihood old/new: %.3v/%.3v\tF-Score old/new: %.3f/%.3f" ll ll' score score')
--   return $! ()

-- -- | Run one sampling iteration
-- evaluateBoundaries :: Locations a -> Maybe [Int] -> Double
-- evaluateBoundaries guesses (Just golds) = (2.0 * precision * recall) / (precision + recall)
--   where
--     guesses' = Set.fromList $ Vector.toList $ Vector.findIndices (\x -> _morphFinal x) guesses
--     golds' = Set.fromList golds
--     trueCount = fromIntegral (Set.size golds')
--     guessCount = fromIntegral (Set.size guesses')
--     correctCount = fromIntegral (Set.size $ guesses' `Set.intersection` golds')
--     precision = correctCount / guessCount
--     recall = correctCount / trueCount

-- likelihood :: (Show elem, Probability p, Show p, MonadIO m, MonadRandom m, (MonadReader (Params p)) m, MonadState (SamplingState elem) m, MonadLog (WithSeverity String) m) => m p
-- likelihood = do
--   SamplingState{..} <- get
--   Params{..} <- ask
--   ps <- sequence $ map (\(w, n) -> oneWordProb _counts _charProb _stop _dontStop _alpha n w) (Map.toList _counts)
--   let p = foldl1' (*) ps
--   return $! p

-- --
-- --   Top-level actions
-- --

-- -- | Train a model on given data
-- train :: (Show p, Show elem, Categorical p, Probability p, MonadIO m, MonadLog (WithSeverity String) m, MonadRandom m, MonadState (SamplingState elem) m, MonadReader (Params p) m) => Vector elem -> Double -> (Params p) -> Int -> StdGen -> m (SamplingState elem)
-- train seq eta params iterations gen = do
--   logInfo (printf "Initial random seed: %v" (show gen))
--   (locations, gold) <- createData params seq  
--   let numChars = (length . nub . map (\x -> _value x) . Vector.toList) locations
--       charProb = fromDouble $ 1.0 / (fromIntegral numChars)
--       (locations', gen') = randomizeLocations eta locations gen
--       counts = initializeCounts locations'
--       (lookupS, lookupE) = initializeLookups locations'
--       state = SamplingState counts locations' lookupS lookupE Set.empty
--       params' = params { _gold=gold, _charProb=charProb }
--   --logNotice (printf "Training model on sequence of length %d" (Vector.length locations'))
--   --logNotice (printf "Training model on initial sequence of length %d, final length %d" (Vector.length seq) (Vector.length locations'))  
--   runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen') state) params'
