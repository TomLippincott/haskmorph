{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

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
import Text.HaskSeg.Probability (Prob, LogProb, Probability(..), showDist, sampleCategorical)
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..))
import Text.HaskSeg.Metrics (f1)
import Data.Char (toLower, isPunctuation)
import Codec.Compression.GZip (compress, decompress)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T


type ProbRep = LogProb

--
--   Command-line parsing
--

logLevels :: Map String Severity
logLevels = Map.fromList [ ("debug", Debug)
                         , ("info", Informational)
                         , ("warn", Warning)
                         , ("error", Error)
                         ]

data Parameters w = Train { input :: w ::: String <?> "Input data file"
                          , model :: w ::: String <?> "Model file"
                          , iterations :: w ::: Int <?> "Number of sampling iterations"
                          , lineCount :: w ::: Maybe Int <?> "Number of lines to read (default: all)"
                          , alphaParam :: w ::: Maybe Double <?> "Per-decision concentration parameter (default: 0.1)"
                          , sharpParam :: w ::: Maybe Double <?> "Probability to stop generating characters when drawing an unseen word (default: 0.5)"
                          , etaParam :: w ::: Maybe Double <?> "Initial probability of each site being a boundary (default: 1.0)"
                          , useSpaces :: w ::: Bool <?> "Make whitespace characters static borders (default: false)"
                          , typeBased :: w ::: Bool <?> "Run over word types, rather than tokens (default: false)"
                          , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                          , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                          , minCount :: w ::: Maybe Int <?> "Only consider words with the given minimum frequency"                          
                          }
                  | Apply { input :: w ::: String <?> "Input data file"
                          , model :: w ::: String <?> "Model file"
                          , iterations :: w ::: Int <?> "Number of sampling iterations"
                          , lineCount :: w ::: Maybe Int <?> "Number of lines to read (default: all)"
                          , labeled :: w ::: String <?> "Output file for labeled data"
                          , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                          , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                          }
                  | Print { model :: w ::: String <?> "Model file"
                          , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"                          
                          , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                          }

  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

goldA = A.setSGRCode [A.SetColor A.Background A.Vivid A.Green]
goldB = A.setSGRCode [A.SetColor A.Background A.Dull A.Green]
goldAlts = [if i `mod` 2 == 0 then goldA else goldB | i <- [1..]]

goldFormat = A.setSGRCode [A.SetColor A.Background A.Vivid A.Blue]
staticFormat = A.setSGRCode [A.SetColor A.Background A.Vivid A.Yellow]
sampleFormat = A.setSGRCode [A.SetColor A.Foreground A.Vivid A.Red]
siteFormat = A.setSGRCode [A.SetUnderlining A.SingleUnderline]
pivotFormat = A.setSGRCode [A.SetConsoleIntensity A.BoldIntensity, A.SetUnderlining A.SingleUnderline]

sampleA = A.setSGRCode [A.SetColor A.Foreground A.Vivid A.Black]
sampleB = A.setSGRCode [A.SetColor A.Foreground A.Vivid A.Red]
sampleAlts = [if i `mod` 2 == 0 then sampleA else sampleB | i <- [1..]]

reset = A.setSGRCode [A.Reset]


showFullState :: (IsChar elem, MonadState (SamplingState elem) m, MonadReader (Params ProbRep) m, PrintfArg elem) => Maybe Int -> Maybe (Set Int) -> m String
showFullState mi ms = do
  SamplingState{..} <- get
  params@(Params{..}) <- ask  
  let ls = (Vector.toList . Vector.indexed) _locations
      renderChar ([], golds, samples) = Nothing
      renderChar ((i, Location{..}):locs, golds, samples) = Just (formatting ++ (printf "%v" _value) ++ reset, (locs, golds', samples'))
        where
          g:gs = golds
          s:ss = samples
          isGold = i `Set.member` _gold
          isSet = _morphFinal
          isPivot = Just i == mi
          isStatic = _static
          isSite = i `Set.member` (fromMaybe Set.empty ms)
          gf = if isGold then Just goldFormat else Nothing
          sf = if isSet then Just sampleFormat else Nothing
          pf = if isPivot then Just pivotFormat else Nothing          
          ssf = if isSite then Just siteFormat else Nothing
          stf = if isStatic then Just staticFormat else Nothing
          formatting = (concat . catMaybes) [gf, sf, pf, ssf, stf]
          golds' = if isGold then gs else g:gs
          samples' = if isSet then ss else s:ss
      toks = unfoldr renderChar (ls, goldAlts, sampleAlts)
  --  return $ concat toks
  return $! (intercalate "\n" [concat toks, printf "Starts: %v" (showLookup _startLookup), printf "Ends: %v" (showLookup _endLookup), printf "Counts: %v" (showCounts _counts)])


-- | The application monad that handles the RNG, sampling state, parameters, and logging
type Sampler elem = RandT StdGen (StateT (SamplingState elem) (ReaderT (Params ProbRep) (LoggingT (WithSeverity String) IO)))
instance (MonadLog (WithSeverity String) m) => MonadLog (WithSeverity String) (RandT g m)

--
--   Word-boundary-lookup-related functions
--

-- | Remove morphs with no associated locations
cleanLookup :: Lookup elem -> Lookup elem
cleanLookup = Map.filter (\x -> Set.size x /= 0)

-- | Initialize word lookup from scratch, given sampling state
initializeLookups :: (Ord a, Show a) => Locations a -> (Lookup a, Lookup a)
initializeLookups ls = go ((Vector.toList . Vector.indexed) ls) Map.empty Map.empty []
  where
    go ((i, l):ls') mS mE w = case _morphFinal l of
                                False -> go ls' mS mE w'
                                True -> go ls' (Map.insertWith (Set.union) (Vector.fromList $ reverse w') (Set.singleton $ i - (length w) - 1) mS) (Map.insertWith (Set.union) (Vector.fromList $ reverse w') (Set.singleton $ i) mE) []
      where
        w' = _value l : w
    go [] mS mE w = (mS, mE)

-- | Compute the start and end lookup updates implied by setting the given sites to positive and negative, based on the two context-words
computeUpdates :: (Ord elem, Show elem) => Set Int -> Set Int -> Morph elem -> Morph elem -> (Lookup elem, Lookup elem)
computeUpdates pos neg a b = (sUp, eUp)
  where
    c = a Vector.++ b
    aLocs = Set.map (\x -> (x - (Vector.length a), x)) pos
    bLocs = Set.map (\x -> (x, x + (Vector.length b))) pos
    cLocs = Set.map (\x -> (x - (Vector.length a), x + (Vector.length b))) neg
    sUp = Map.fromListWith Set.union [(w, Set.map fst ls) | (w, ls) <- zip [a, b, c] [aLocs, bLocs, cLocs]]
    eUp = Map.fromListWith Set.union [(w, Set.map snd ls) | (w, ls) <- zip [a, b, c] [aLocs, bLocs, cLocs]]

--
--   Location/Site-related functions
--
    
createData :: (MonadLog (WithSeverity String) m) => (Params ProbRep) -> Vector Char -> m (Locations Char, Set Int)
createData Params{..} cs' = do
  let cs = Vector.toList cs'
      ls = lines cs
      wss = concat $ map words ls
      wc = Map.fromListWith (\a b -> a + b) (zip wss $ repeat 1)
      keep = Map.filter (>= _minCount) wc
      ws = if _types == True then Map.keys keep else concat $ map words ls
      bs = map length ws
      bs' = (reverse . drop 1 . reverse . drop 1) $ scanl (+) (-1) bs
      ws' = if _spaces == True then ws else [concat ws]
      ws'' = Vector.concat [sequenceToLocations w | w <- ws']
  logInfo (printf "Loaded data set of %d characters/%d words" (length cs) (length ws))
  return $! (ws'', Set.fromList bs')

randomFlip p g = (v < p, g')
  where
    (v, g') = randomR (0.0, 1.0) g

-- | Switch each potential morpheme boundary (i.e. intra-word indices) to True or False
randomizeLocations :: Double -> Locations elem -> StdGen -> (Locations elem, StdGen)
randomizeLocations p xs g = (Vector.fromList xs', g')
  where
    (g', bs) = mapAccumL (\g'' Location{..} -> if _static == True then (g'', True) else swap (randomFlip p g'' :: (Bool, StdGen))) g (Vector.toList xs)
    xs' = [x { _morphFinal=b } | (x, b) <- zip (Vector.toList xs) bs]

updateLocations' :: elem -> Locations elem -> Set Int -> Set Int -> Locations elem
updateLocations' a ls pos neg = Vector.update ls updates
  where
    p = Location a True False
    n = Location a False False
    pos' = (Vector.map (\i -> (i, p)) . Vector.fromList . Set.toList) pos
    neg' = (Vector.map (\i -> (i, n)) . Vector.fromList . Set.toList) neg
    updates = pos' Vector.++ neg'

updateLocations :: (MonadState (SamplingState elem) m) => elem -> Set Int -> Set Int -> m ()
updateLocations a pos neg = do
  --Vector.update ls updates
  let p = Location a True False
      n = Location a False False
      pos' = (Vector.map (\i -> (i, p)) . Vector.fromList . Set.toList) pos
      neg' = (Vector.map (\i -> (i, n)) . Vector.fromList . Set.toList) neg
      updates = pos' Vector.++ neg'
  modify' (\state -> state)
    
-- | Turn a sequence of values into a sequence of locations
sequenceToLocations :: [elem] -> Locations elem
sequenceToLocations xs = Vector.fromList $ nonFinal ++ [final]
  where
    xs' = init xs
    nonFinal = map (\x -> Location x False False) xs'
    x = last xs
    final = Location x True True

-- | Find the two words implied by a boundary at the given site
siteToWords' :: (Show elem, MonadLog (WithSeverity String) m) => Locations elem -> Int -> m (Morph elem, Morph elem)
siteToWords' ls s = do
  let (before, after) = Vector.splitAt (s + 1) ls
      (bPref, bRem) = Vector.break _morphFinal after
      (b', before') = Vector.splitAt 1 (Vector.reverse before)
      (aPref, aRem) = Vector.break _morphFinal before'
      b = case Vector.length bRem of 0 -> bPref
                                     _ -> bPref Vector.++ (Vector.fromList [Vector.head bRem])
      (before'', after'') = (Vector.map _value (Vector.reverse (b' Vector.++ aPref)), Vector.map _value b)
  return $! (before'', after'')

-- | Find the two words implied by a boundary at the given site
siteToWords :: (Show elem, MonadLog (WithSeverity String) m, MonadState (SamplingState elem) m) => Int -> m (Morph elem, Morph elem)
siteToWords s = do
  SamplingState{..} <- get
  let ls = _locations
  let (before, after) = Vector.splitAt (s + 1) ls
      (bPref, bRem) = Vector.break _morphFinal after
      (b', before') = Vector.splitAt 1 (Vector.reverse before)
      (aPref, aRem) = Vector.break _morphFinal before'
      b = case Vector.length bRem of 0 -> bPref
                                     _ -> bPref Vector.++ (Vector.fromList [Vector.head bRem])
      (before'', after'') = (Vector.map _value (Vector.reverse (b' Vector.++ aPref)), Vector.map _value b)
  return $! (before'', after'')


-- | For sites with matching type, return a subset that don't conflict
nonConflicting :: (MonadLog (WithSeverity String) m) => (Int, (Int, Int)) -> Set (Int, (Int, Int)) -> Set (Int, (Int, Int)) -> m (Set Int, Set Int)
nonConflicting piv@(pivi, (si1, si2)) a b = return $! (a'', b'')
  where
    reducer (ms, vs) (i, (s1, s2)) = (ms', vs')
      where
        affected = Set.fromList [s1..s2]
        conflict = Set.size (ms `Set.intersection` affected) > 0
        ms' = if conflict then ms else ms `Set.union` affected
        vs' = if conflict then vs else i `Set.insert` vs
    (mods, a') = Set.foldl' reducer (Set.fromList [si1..si2], Set.empty) a
    (mods', b') = Set.foldl' reducer (mods, Set.empty) b
    a'' = if piv `Set.member` a then pivi `Set.insert` a' else a'
    b'' = if piv `Set.member` b then pivi `Set.insert` b' else b'
    
-- | For two words, return all compatible sites
wordsToSites :: (IsChar elem, MonadState (SamplingState elem) m, MonadReader (Params ProbRep) m, MonadLog (WithSeverity String) m, Show elem, Ord elem, PrintfArg elem) => Int -> Lookup elem -> Lookup elem -> Morph elem -> Morph elem -> m (Set Int, Set Int)
wordsToSites piv luS luE a b = do
  let j = a Vector.++ b
      jS = Vector.fromList $ map (\x -> x + (Vector.length a)) (Set.toList $ Map.findWithDefault Set.empty j luS)
      aE = Map.findWithDefault Set.empty a luE
      bS = Map.findWithDefault Set.empty b luS
      splits' = Set.map (\i -> (i, (i - length a, i + length b))) $ Set.intersection aE bS
      nonSplits' = Set.map (\i -> (i, (i - length a, i + length b))) $  (Set.fromList . Vector.toList) jS
      piv' = (piv, (piv - length a, piv + length b))
  (splits, nonSplits) <- nonConflicting piv' splits' nonSplits'
  let nSplit = Set.size splits
      nFull = Set.size nonSplits
  --s <- showFullState Nothing Nothing
  --if nSplit + nFull == 0 then (logDebug s) >> (logDebug $ show (luS, luE)) >> error "Found zero sites!" else return ()
  return $! (nonSplits, splits)

--
--   Sampling-distribution-related functions
--

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
  
-- | 
combinations :: (MonadLog (WithSeverity String) m, Show p, Probability p) => Int -> m (Vector p)
combinations n = do
  let cs = Vector.generate (n + 1) (fromDouble . fromIntegral . (n `choose`))
  --logDebug (printf "comb(%d) = %v" n (show cs))
  return $! cs
    --tot = fromIntegral $ Vector.sum cs
    --cs' = Vector.map (\c -> fromDouble $ (fromIntegral c) / tot) cs

-- | Compute the log-categorical distribution of possible number of sites to set to positive:
--     P(m) = (n choose m) * g(m)
distribution :: (Show p, MonadLog (WithSeverity String) m, Probability p, Show elem, Ord elem, Show p) => Counts elem -> p -> p -> p -> Morph elem -> Morph elem -> p -> Int -> m (Vector p)
distribution counts charProb stopProb dontStopProb before after alpha n = do
  gs <- (liftM Vector.fromList . sequence) [g counts charProb stopProb dontStopProb before after alpha n m | m <- [0..n]]
  combs <- combinations n
  let unScaled = Vector.map (\(x, y) -> x * y) (Vector.zip combs gs)
  return $! unScaled
  
--
--   Count-related functions
--

-- | Remove zero-elements from a count object
cleanCounts :: Counts elem -> Counts elem
cleanCounts = Map.filter (\x -> x /= 0)

-- | Initialize word counts from scratch, given boundary assignments
initializeCounts :: (Ord elem, Show elem) => Locations elem -> Counts elem
initializeCounts ls = Map.fromListWith (+) (Vector.toList (Vector.map (\x -> (x, 1)) words'))
  where
    words = Vector.unfoldr (\xs -> case span (\x -> _morphFinal x == False) xs of
                               ([], []) -> Nothing
                               (xs', x:ys) -> Just (xs' ++ [x], ys)
                           ) (Vector.toList ls)
    words' = Vector.map (Vector.fromList . map _value) words

-- | Use provided function to update counts for a word
updateCounts :: (Ord elem) => (Int -> Int -> Int) -> Morph elem -> Int -> Counts elem -> Counts elem
updateCounts f w n = Map.insertWith f w n

-- | Convenience function for adding counts
addCounts = updateCounts (+)

-- | Convenience function for subtracting counts
subtractCounts = updateCounts (flip (-))

--
--   Sampling-mechanism-related functions
--

-- | Randomly sample a site from those currently available, and then block-sample all compatible sites, returning the updated list of available sites
sampleSite :: (MonadIO m, MonadLog (WithSeverity String) m, MonadRandom m, MonadState (SamplingState Char) m, MonadReader (Params ProbRep) m) => Set Int -> m (Set Int)
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
  s <- showFullState (Just i) (Just sites)
  logDebug s
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
  s' <- showFullState Nothing Nothing
  logDebug s'
  return $! ix Set.\\ sites

-- | Run one sampling iteration
sample :: (MonadIO m, MonadRandom m, (MonadReader (Params ProbRep)) m, MonadState (SamplingState Char) m, MonadLog (WithSeverity String) m) => Int -> m ()
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

-- | Run one sampling iteration
evaluateBoundaries :: Locations a -> Maybe [Int] -> Double
evaluateBoundaries guesses (Just golds) = (2.0 * precision * recall) / (precision + recall)
  where
    guesses' = Set.fromList $ Vector.toList $ Vector.findIndices (\x -> _morphFinal x) guesses
    golds' = Set.fromList golds
    trueCount = fromIntegral (Set.size golds')
    guessCount = fromIntegral (Set.size guesses')
    correctCount = fromIntegral (Set.size $ guesses' `Set.intersection` golds')
    precision = correctCount / guessCount
    recall = correctCount / trueCount

likelihood :: (MonadIO m, MonadRandom m, (MonadReader (Params ProbRep)) m, MonadState (SamplingState Char) m, MonadLog (WithSeverity String) m) => m ProbRep
likelihood = do
  SamplingState{..} <- get
  Params{..} <- ask
  ps <- sequence $ map (\(w, n) -> oneWordProb _counts _charProb _stop _dontStop _alpha n w) (Map.toList _counts)
  let p = foldl1' (*) ps
  return $! p

--
--   Top-level actions
--

-- | Train a model on given data
train :: (MonadIO m, MonadLog (WithSeverity String) m) => Vector Char -> Double -> (Params ProbRep) -> Int -> StdGen -> m (SamplingState Char)
train seq eta params iterations gen = do
  logInfo (printf "Initial random seed: %v" (show gen))
  (locations, gold) <- createData params seq  
  let numChars = (length . nub . map (\x -> _value x) . Vector.toList) locations
      charProb = fromDouble $ 1.0 / (fromIntegral numChars)
      (locations', gen') = randomizeLocations eta locations gen
      counts = initializeCounts locations'
      (lookupS, lookupE) = initializeLookups locations'
      state = SamplingState counts locations' lookupS lookupE Set.empty
      params' = params { _gold=gold, _charProb=charProb }
  --logNotice (printf "Training model on sequence of length %d" (Vector.length locations'))
  --logNotice (printf "Training model on initial sequence of length %d, final length %d" (Vector.length seq) (Vector.length locations'))  
  runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen') state) params'


--
--   Main entrypoint
--

split' :: (a -> Bool) -> [a] -> [[a]]
split' p xs = go xs []
  where
    go [] acc = acc
    go xs' acc = go rest (x:acc)
      where
        (pref, r:rest) = span p xs'
        x = pref ++ [r]

formatWord :: [Location Char] -> String
formatWord ls = printf "%s : %s" word (intercalate "@@" (reverse (morphs ls [])))
  where
    word = map _value ls
    morphs [] acc = acc
    morphs ls' acc = morphs rem (morph:acc)
      where
        (pref, r:rem) = span (\x -> _morphFinal x == False) ls'
        morph = map _value (pref ++ [r]) 

showLexicon :: Locations Char -> [String]
showLexicon ls = go [] (Vector.toList ls)
  where
    go acc [] = acc
    go acc ls' = go (word:acc) rem
      where
        (pref, r:rem) = span (\x -> _static x == False) ls'
        word = formatWord (pref ++ [r]) 

toLookup :: Locations Char -> Map String [String]
toLookup ls = Map.fromList ls''
  where
    ls' = split' (\x -> _static x == False) (Vector.toList ls)
    ls'' = [(map _value l, reverse $ map (map _value) (split' (\x -> _morphFinal x == False) l)) | l <- ls']    

main :: IO ()
main = do
  args <- unwrapRecord "Type-based sampling for segmentation model with Dirichlet process prior on words"
  gen <- case randomSeed args of Nothing -> getStdGen
                                 Just i -> return $! mkStdGen i
  let level = logLevels Map.! (fromMaybe "info" (logLevel args))

  runLoggingT (case args of
                  Train{..} -> do
                    seq' <- liftIO $ (liftM (Vector.fromList . intercalate " " . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) input
                    let seq = seq' -- (Vector.map toLower . Vector.filter (\c -> not (isPunctuation c))) seq'
                        numChars = (length . nub . Vector.toList) seq
                        charProb = fromDouble $ 1.0 / (fromIntegral numChars)
                        params = Params (fromDouble $ fromMaybe 0.1 alphaParam) (fromDouble $ fromMaybe 0.5 sharpParam) (fromDouble $ 1.0 - (fromMaybe 0.5 sharpParam)) useSpaces typeBased Set.empty charProb (fromMaybe 1 minCount)
                    state <- train seq (fromMaybe 1.0 etaParam) params iterations gen
                    liftIO $ BS.writeFile model ((compress . T.encodeUtf8 . T.pack . show) $ (params, _locations state))
                  Print{..} -> do
                    (params :: (Params ProbRep), modelLocations :: Locations Char) <- (liftIO $ (liftM read . liftM T.unpack . liftM T.decodeUtf8 . liftM decompress . BS.readFile) model)
                    liftIO $ putStrLn (intercalate "\n" (showLexicon modelLocations))
                  Apply{..} -> do
                    seq <- liftIO $ (liftM (Vector.fromList . concat . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) input
                    (params :: (Params ProbRep), modelLocations :: Locations Char) <- (liftIO $ (liftM read . liftM T.unpack . liftM T.decodeUtf8 . liftM decompress . BS.readFile) model)
                    let lu = toLookup modelLocations
                    liftIO $ print 10
                    --liftIO $ print lu
              )
                    --ls <- apply params (Vector.fromList [x | x <- Vector.toList modelLocations]) seq iterations gen
                    --ls <- apply params (Vector.fromList [x {_static=True } | x <- Vector.toList modelLocations]) seq iterations gen                    
                    --liftIO $ writeFile labeled (show ls))
                    (\msg -> case msgSeverity msg <= level of
                               True -> putStrLn (discardSeverity msg)
                               False -> return $! ()
                      )

  A.setSGR [A.Reset]
