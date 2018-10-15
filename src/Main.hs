{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (lookup)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM, foldM)
import System.IO (withFile, hPutStr, IOMode(..), readFile)
import System.Random (getStdGen, getStdRandom, newStdGen, RandomGen, mkStdGen, StdGen)
import Data.List (unfoldr, nub, mapAccumL, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import VectorShuffling.Immutable (shuffle)
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf)
import Debug.Trace (traceShow, traceShowId)
import Math.Combinatorics.Exact.Binomial (choose)
import Control.Monad.Loops
import Control.Monad.Log
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Reader.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Tuple (swap)
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Random
import Control.Monad.RWS
import Control.Lens hiding (Wrapped, Unwrapped)


data Parameters w = Train { training :: w ::: String <?> "Training data file"
                          , lineCount :: w ::: Int <?> "Number of lines to read"
                          , iterations :: w ::: Int <?> "Number of sampling iterations"                          
                          , alphaParam :: w ::: Double <?> "Per-decision concentration parameter (0.1)"
                          , stopParam :: w ::: Double <?> "Probability to stop generating characters when drawing an unseen word"
                          , modelOutput :: w ::: String <?> "Output file for scores"                          
                          }
                  | Apply { modelFile :: w ::: String <?> "Model file (output or input, depending on whether training or testing, respectively)"
                          , testing :: w ::: String <?> "Testing data file"
                          , labeledOutput :: w ::: String <?> "Output file for scores"
                          }
  deriving (Generic)                              


instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)


data Location a = Location { _value :: a
                           , _morphFinal :: Bool
                           , _static :: Bool
                           , _offset :: Int
                           } deriving (Show, Read)

type Counts a = Map [a] Int
type Lookup a = Map [a] (Set Int)
type Locations a = [Location a]

data SamplingState a = SamplingState { _counts :: Counts a
                                     , _locations :: Locations a
                                     , _startLookup :: Lookup a
                                     , _endLookup :: Lookup a
                                     } deriving (Show, Read)

makeLenses ''SamplingState

data Params = Params { _alpha :: Double
                     , _stop :: Double
                     } deriving (Show, Read)

defaultParams = Params 0.1 0.1
makeLenses ''Params

type Sampler elem = RandT StdGen (StateT (SamplingState elem) (ReaderT Params (LoggingT (WithSeverity String) IO)))
instance (MonadLog (WithSeverity String) m) => MonadLog (WithSeverity String) (RandT g m)


showLocations :: [Location Char] -> [Char]
showLocations ls = concat toks
  where
    toks = map (\x -> if _morphFinal x && _static x then [_value x, '+', ' '] else if _morphFinal x then [_value x, '|', ' '] else [_value x, ' ', ' ']) ls


sequenceToLocations :: [a] -> [Location a]
sequenceToLocations xs = nonFinal ++ [final]
  where
    xs' = init xs
    nonFinal = map (\x -> Location x False False (-1)) xs'
    x = last xs
    final = Location x True True (-1)


-- | Switch each potential morpheme boundary (i.e. intra-word indices) to
--   True or False, uniformly at random
randomizeLocations :: [Location a] -> StdGen -> ([Location a], StdGen)
randomizeLocations xs g = (xs', g')
  where
    (g', bs) = mapAccumL (\g'' Location{..} -> if _static == True then (g'', True) else swap (random g'' :: (Bool, StdGen))) g xs
    --[if static == True then True else (fst $ random g) :: Bool | Location{..} <- xs]
    --bs <- sequence $ [if static == True then return True else randomIO :: IO Bool | Location{..} <- xs]
    xs' = [x { _morphFinal=b } | (x, b) <- zip xs bs]


-- | Initialize word counts from scratch, given sampling state
initializeCounts :: (Ord a, Show a) => Locations a -> Counts a
initializeCounts ls = Map.fromListWith (+) (map (\x -> (x, 1)) words')
  where
    words = unfoldr (\xs -> case span (\x -> _morphFinal x == False) xs of
                              ([], []) -> Nothing
                              (xs', x:ys) -> Just (xs' ++ [x], ys)
                    ) ls
    words' = map (map _value) words


-- | Initialize word lookup from scratch, given sampling state
initializeLookups :: (Ord a, Show a) => Locations a -> (Lookup a, Lookup a)
initializeLookups ls = go ls Map.empty Map.empty []
  where
    go (l:ls') mS mE w = case _morphFinal l of
                       False -> go ls' mS mE w'
                       True -> go ls' (Map.insertWith (Set.union) (reverse w') (Set.singleton $ _offset l - (length w + 1)) mS) (Map.insertWith (Set.union) (reverse w') (Set.singleton $ _offset l) mE) []
      where
        w' = _value l : w
    go [] mS mE w = (mS, mE)


categorical :: (MonadRandom m) => [Double] -> m Int
categorical ps = do
  let s = sum ps
      ps' = map (\x -> x / s) ps
      ps'' = scanl (+) 0.0 ps'
  v <- getRandomR (0.0, 1.0 :: Double)
  return (length (takeWhile (\x -> x < v) ps'') - 1)


siteToSet :: Locations a -> Lookup a -> Int -> [Int]
siteToSet st lu i = []


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f [x] = if f x then [x] else error "Final item not a boundary!"
takeUntil f (x:xs) = if f x then [x] else (x:takeUntil f xs)


siteToWords :: Locations a -> Int -> ([a], [a])
siteToWords st s = (map _value a, map _value b)
  where    
    after = drop (s + 1) st
    b = takeUntil (\x -> _morphFinal x) after
    (i:before) = reverse (take (s + 1) st)
    a = drop 1 $ reverse $ i : (takeUntil (\x -> _morphFinal x) before)

    
wordsToSites :: (Ord a) => Lookup a -> Lookup a -> [a] -> [a] -> ([Int], [Int])
wordsToSites luS luE a b = (jS, splits)
  where
    j = a ++ b
    jS = map (\x -> x + (length a)) (Set.toList $ Map.findWithDefault Set.empty j luS)
    aE = Map.findWithDefault Set.empty a luE
    bS = Map.findWithDefault Set.empty b luS
    splits = Set.toList $ Set.intersection aE bS


oneWordProb :: (Show a, Ord a) => Counts a -> Double -> Double -> Int -> [a] -> Double
oneWordProb counts stopProb alpha reps word = numer / denom
  where
    mu = ((1.0 - stopProb) ^ (length word)) * stopProb
    total = fromIntegral $ sum $ Map.elems counts
    count = fromIntegral $ Map.findWithDefault 0 word counts
    numer = ((alpha * mu) + count) ^ reps
    denom = (alpha + total) ^ reps    


g :: (Ord a, Show a) => Counts a -> Double -> [a] -> [a] -> Double -> Int -> Int -> Double
g counts stopProb before after alpha n m= posProb * negProb
  where
    beforeProb = oneWordProb counts stopProb alpha m before
    afterProb = oneWordProb counts stopProb alpha m after
    posProb = beforeProb * afterProb
    negProb = oneWordProb counts stopProb alpha (n - m) (before ++ after)
    

dist :: (Show a, Ord a) => Counts a -> Double -> [a] -> [a] -> Double -> Int -> [Double]
dist counts stopProb before after alpha n = [p / total | p <- unScaled]
  where
    combinations = [fromIntegral (n `choose` m) | m <- [0..n]]
    gs = [g counts stopProb before after alpha n m | m <- [0..n]]
    unScaled = map (\(x, y) -> x * y) (zip combinations gs)
    total = sum unScaled
    

sampleSite :: (MonadIO m, Show elem, Ord elem, MonadLog (WithSeverity String) m, MonadRandom m, MonadState (SamplingState elem) m, MonadReader Params m) => Set Int -> m (Set Int)
sampleSite ix = do
  i <- (liftM (Set.toList ix !!) (getRandomR (0, Set.size ix - 1)))
  Params{..} <- ask
  SamplingState{..} <- get
  let (a, b) = siteToWords _locations i
      c = a ++ b
      (fullSites, splitSites) = wordsToSites _startLookup _endLookup a b
      sites = fullSites ++ splitSites
      
      cs' = Map.insertWith (flip (-)) c (length fullSites) _counts
      cs'' = Map.insertWith (flip (-)) a (length splitSites) cs'
      cs''' = Map.insertWith (flip (-)) b (length splitSites) cs''
      --printf "Matching sites (full/split): %s/%s\n" (show fullSites) (show splitSites)      
      --printf "Decremented counts: %s\n" $ showCounts cs'''
  
      d = (dist cs''' _stop a b _alpha (length sites))
  numPos <- categorical d
--   --printf "Probabilities: %s\n" (show d)
--   --printf "Setting %d/%d locations to positive\n" numPos (length sites)
  g <- liftIO getStdGen
  let (allIndices, _) = shuffle (Vector.fromList sites) g
      (pos, neg) = splitAt numPos (Vector.toList allIndices)
      pos' = Set.fromList pos
      neg' = Set.fromList neg
      cs'''' = Map.insertWith (+) c (length neg) cs'''
      cs''''' = Map.insertWith (+) a (length pos) cs''''
      cs'''''' = Map.insertWith (+) b (length pos) cs'''''
      cs''''''' = Map.fromList $ [(k, v) | (k, v) <- Map.toList cs'''''', v /= 0]
      st' = [Location {_value=_value, _morphFinal=if _offset `Set.member` pos' then True else if _offset `Set.member` neg' then False else _morphFinal, _static=_static, _offset=_offset} | Location{..} <- _locations]
--       -- FIX!
      (luS', luE') = initializeLookups st'
  --logMessage (WithSeverity Debug (printf "  Chose pivot at %d, with %s to the left, %s to the right (%d)" i (show a) (show b) (Set.size ix)))
  return $ ix Set.\\ (Set.fromList sites)


sample :: (MonadIO m, MonadRandom m, (MonadReader Params) m, MonadState (SamplingState Char) m, MonadLog (WithSeverity String) m) => Int -> m ()
sample i = do
  logMessage (WithSeverity Informational (printf "Iteration #%d" i))
  state <- get
  params <- ask
  let indices = Set.fromList [i | (l, i) <- zip (view locations state) [0..], _static l == False]
  iterateUntilM (\s -> Set.size s == 0) sampleSite indices
  --logMessage (WithSeverity Debug (showLocations $ view locations state))
  return ()


perplexity :: (MonadIO m, MonadRandom m, (MonadReader Params) m, MonadState (SamplingState Char) m, MonadLog (WithSeverity String) m) => m Double
perplexity = return 1.0


train :: [[Char]] -> Params -> Int -> IO (SamplingState Char)
train sequences params iterations = do
  gen <- getStdGen
  let locations' = (Location '#' True True (-1)) : (concat $ map sequenceToLocations sequences)
      locations'' = [l { _offset=i } | (i, l) <- zip [0..] locations']
      (locations, gen') = randomizeLocations locations'' gen
      counts = initializeCounts locations
      (lookupS, lookupE) = initializeLookups locations
      state = SamplingState counts locations lookupS lookupE  
  state' <- (runLoggingT (runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen') state) params) (\x -> putStrLn (discardSeverity x)))
  return state'


apply :: Locations Char -> [[Char]] -> IO ()
apply model sequences = do
  let locations' = (Location '#' True True (-1)) : (concat $ map sequenceToLocations sequences)
      locations = [l { _offset=i } | (i, l) <- zip [0..] locations']
      counts = initializeCounts locations
      (lookupS, lookupE) = initializeLookups locations
      state = SamplingState counts locations lookupS lookupE  
  --state' <- (runLoggingT (runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen') state) defaultParams) (\x -> putStrLn (discardSeverity x)))
  return ()
  

main :: IO ()
main = do
  args <- unwrapRecord "Type-based sampling for morphological models with Dirichlet process prior on words"
  case args of
    Train{..} -> do
      words <- (liftM (nub . concat . map words . take lineCount . lines) . readFile) training
      let params = Params alphaParam stopParam
      state <- train words params iterations
      writeFile modelOutput (show $ (params, view locations state))
    Apply{..} -> do
      words <- (liftM (nub . concat . map words . lines) . readFile) testing
      (params, modelLocations) <- (liftM read . readFile) modelFile :: IO (Params, Locations Char)
      ls <- apply modelLocations words
      writeFile labeledOutput (show ls)
      return ()
