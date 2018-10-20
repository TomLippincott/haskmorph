{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (lookup)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM, foldM)
import System.IO (withFile, hPutStr, IOMode(..), readFile)
import System.Random (getStdGen, mkStdGen)
import Data.List (unfoldr, nub, mapAccumL, intercalate, sort)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf, PrintfArg)
import Math.Combinatorics.Exact.Binomial (choose)
import Control.Monad.Loops
import Control.Monad.Log
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Reader.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Tuple (swap)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Random
import System.Random.Shuffle (shuffleM)

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
                          -- , baseDistribution :: w ::: SomeType <?> "Potentially, infer character probabilities etc"
                          }
                  | Apply { input :: w ::: String <?> "Input data file"
                          , model :: w ::: String <?> "Model file"
                          , iterations :: w ::: Int <?> "Number of sampling iterations"
                          , lineCount :: w ::: Maybe Int <?> "Number of lines to read (default: all)"
                          , labeled :: w ::: String <?> "Output file for labeled data"
                          , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                          , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                          }
  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

--
--   Type-level definitions
--

type Prob = Double
type Dist = [Double]
newtype LogProb = LogProb Double deriving (Show, Read, Eq, Ord, PrintfArg, Fractional)
newtype LogDist = LogDist [LogProb]

logProb :: Prob -> LogProb
logProb p = LogProb (logBase 2 p)

prob :: LogProb -> Prob
prob (LogProb lp) = 2 ** lp

instance Num LogProb where
  (LogProb a) + (LogProb b) = LogProb (l + (logBase 2 v))
    where
      (l, s) = if a > b then (a, b) else (b, a)
      d = s - l
      v = 1 + (2 ** d)
  (-) = undefined
  (LogProb a) * (LogProb b) = LogProb (a + b)
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger i = LogProb (fromIntegral i)

type Locations a = [Location a]
type Counts a = Map [a] Int
type Site = Int

data Location a = Location { _value :: a
                           , _morphFinal :: Bool
                           , _static :: Bool
                           , _offset :: Int
                           } deriving (Show, Read)

-- | A "start" lookup points to the boundary *before* the first item, an "end" lookup points to the boundary *of* the last item
type Lookup a = Map [a] (Set Int)

-- | A coherent state of boundary assignments, counts, and word start/end lookups
data SamplingState a = SamplingState { _counts :: Counts a
                                     , _locations :: Locations a
                                     , _startLookup :: Lookup a
                                     , _endLookup :: Lookup a
                                     } deriving (Show, Read)

-- | Parameters that are set at training time
data Params = Params { _alpha :: Double
                     , _stop :: Double
                     , _spaces :: Bool
                     , _types :: Bool
                     } deriving (Show, Read)

-- | The application monad that handles the RNG, sampling state, parameters, and logging
type Sampler elem = RandT StdGen (StateT (SamplingState elem) (ReaderT Params (LoggingT (WithSeverity String) IO)))
instance (MonadLog (WithSeverity String) m) => MonadLog (WithSeverity String) (RandT g m)

--
--   Word-boundary-lookup-related functions
--

-- | Initialize word lookup from scratch, given sampling state
initializeLookups :: (Ord a, Show a) => Locations a -> (Lookup a, Lookup a)
initializeLookups ls = go ls Map.empty Map.empty []
  where
    go (l:ls') mS mE w = case _morphFinal l of
                       False -> go ls' mS mE w'
                       True -> go ls' (Map.insertWith (Set.union) (reverse w') (Set.singleton $ _offset l - (length w) - 1) mS) (Map.insertWith (Set.union) (reverse w') (Set.singleton $ _offset l) mE) []
      where
        w' = _value l : w
    go [] mS mE w = (mS, mE)

-- | Compute the start and end lookup updates implied by setting the given sites to positive and negative, based on the two context-words
computeUpdates :: (Ord elem, Show elem) => Set Int -> Set Int -> [elem] -> [elem] -> (Lookup elem, Lookup elem)
computeUpdates pos neg a b = (sUp, eUp)
  where
    c = a ++ b
    aLocs = Set.map (\x -> (x - (length a), x)) pos
    bLocs = Set.map (\x -> (x, x + (length b))) pos
    cLocs = Set.map (\x -> (x - (length a), x + (length b))) neg
    sUp = Map.fromListWith Set.union [(w, Set.map fst ls) | (w, ls) <- zip [a, b, c] [aLocs, bLocs, cLocs]]
    eUp = Map.fromListWith Set.union [(w, Set.map snd ls) | (w, ls) <- zip [a, b, c] [aLocs, bLocs, cLocs]]

--
--   Location/Site-related functions
--

createData :: Params -> [Char] -> Locations Char
createData Params{..} cs = ls'
  where
    ws = (if _types == True then nub else id) $ if _spaces == True then words cs else [cs]
    ls = concat [sequenceToLocations w | w <- ws]
    ls' = [l { _offset=i } | (i, l) <- zip [0..] ls]

-- | Switch each potential morpheme boundary (i.e. intra-word indices) to True or False
randomizeLocations :: [Location a] -> StdGen -> ([Location a], StdGen)
randomizeLocations xs g = (xs', g')
  where
    (g', bs) = mapAccumL (\g'' Location{..} -> if _static == True then (g'', True) else swap (random g'' :: (Bool, StdGen))) g xs
    xs' = [x { _morphFinal=b } | (x, b) <- zip xs bs]

-- | Pretty-print a sequence of locations
showLocations :: [Location Char] -> [Char]
showLocations ls = unlines [concat toks, concat indices]
  where
    toks = map (\x -> if _morphFinal x && _static x then [_value x, '+', ' '] else if _morphFinal x then [_value x, '|', ' '] else [_value x, ' ', ' ']) ls    
    indices = [printf "%3d" (_offset l) | l <- ls]

-- | Turn a sequence of values into a sequence of locations
sequenceToLocations :: [a] -> [Location a]
sequenceToLocations xs = nonFinal ++ [final]
  where
    xs' = init xs
    nonFinal = map (\x -> Location x False False (-1)) xs'
    x = last xs
    final = Location x True True (-1)

-- | Find the two words implied by a boundary at the given site
siteToWords :: (Show a) => Locations a -> Int -> ([a], [a])
siteToWords ls s = (map _value (reverse (b':aPref)), map _value b)
  where
    (before, after) = splitAt (s + 1) ls
    (bPref, bRem) = break _morphFinal after
    (b':before') = reverse before
    (aPref, aRem) = break _morphFinal before'
    b = case bRem of [] -> bPref
                     (c:_) -> bPref ++ [c]

-- | For two words, return all compatible sites
wordsToSites :: (MonadLog (WithSeverity String) m) => (Ord a) => Lookup a -> Lookup a -> [a] -> [a] -> m (Set Int, Set Int)
wordsToSites luS luE a b = do
  let j = a ++ b
      jS = map (\x -> x + (length a)) (Set.toList $ Map.findWithDefault Set.empty j luS)
      aE = Map.findWithDefault Set.empty a luE
      bS = Map.findWithDefault Set.empty b luS
      splits = Set.intersection aE bS
  return (Set.fromList jS, splits)

--
--   Sampling-distribution-related functions
--

-- | Sample a value from a log-categorical distribution
categorical :: (MonadRandom m) => LogDist -> m Int
categorical (LogDist lps) = do
  let ps = scanl (+) (logProb 0.0) lps
      ps' = map prob ps
  v <- getRandomR (0.0, last ps' :: Double)
  return (length (takeWhile (\x -> x < v) ps') - 1)

-- | Compute the log-probability of generating the given word n times, based on counts
oneWordProb :: (Show a, Ord a) => Counts a -> Double -> Double -> Int -> [a] -> LogProb
oneWordProb counts stopProb alpha n word = LogProb (numer / denom)
  where
    mu = ((1.0 - stopProb) ^ (length word)) * stopProb
    total = fromIntegral $ sum $ Map.elems counts
    count = fromIntegral $ Map.findWithDefault 0 word counts
    numer = ((alpha * mu) + count) ^ n
    denom = (alpha + total) ^ n    

-- | Compute the log-probability of setting a single set of m sites, out of n, to positive
g :: (Ord a, Show a) => Counts a -> Double -> [a] -> [a] -> Double -> Int -> Int -> LogProb
g counts stopProb before after alpha n m= posProb * negProb
  where
    beforeProb = oneWordProb counts stopProb alpha m before
    afterProb = oneWordProb counts stopProb alpha m after
    posProb = beforeProb * afterProb
    negProb = oneWordProb counts stopProb alpha (n - m) (before ++ after)

-- | Compute the log-categorical distribution of possible number of sites to set to positive
dist :: (Show a, Ord a) => Counts a -> Double -> [a] -> [a] -> Double -> Int -> LogDist
dist counts stopProb before after alpha n = LogDist unScaled
  where
    combinations = [(n `choose` m) | m <- [0..n]]
    gs = [g counts stopProb before after alpha n m | m <- [0..n]]
    unScaled = map (\(x, y) -> foldl (+) (logProb 0.0) (replicate x y)) (zip combinations gs)

--
--   Count-related functions
--

-- | Remove zero-elements from a count object
clean = Map.filter (\x -> Set.size x /= 0)

-- | Initialize word counts from scratch, given boundary assignments
initializeCounts :: (Ord a, Show a) => Locations a -> Counts a
initializeCounts ls = Map.fromListWith (+) (map (\x -> (x, 1)) words')
  where
    words = unfoldr (\xs -> case span (\x -> _morphFinal x == False) xs of
                              ([], []) -> Nothing
                              (xs', x:ys) -> Just (xs' ++ [x], ys)
                    ) ls
    words' = map (map _value) words

-- | Use provided function to update counts for a word
updateCounts :: (Ord elem) => (Int -> Int -> Int) -> [elem] -> Int -> Counts elem -> Counts elem
updateCounts f w n = Map.insertWith f w n

-- | Convenience function for adding counts
addCounts = updateCounts (+)

-- | Convenience function for subtracting counts
subtractCounts = updateCounts (flip (-))

--
--   Sampling-mechanism-related functions
--

-- | Randomly sample a site from those currently available, and then block-sample all compatible sites, returning the updated list of available sites
sampleSite :: (MonadIO m, MonadLog (WithSeverity String) m, MonadRandom m, MonadState (SamplingState Char) m, MonadReader Params m) => Set Int -> m (Set Int)
sampleSite ix = do
  Params{..} <- ask
  SamplingState{..} <- get
  logDebug (printf "%d sites remaining" (Set.size ix))
  i <- (liftM (Set.toList ix !!) (getRandomR (0, Set.size ix - 1)))
  logDebug (printf "Chose site %d" i)
  (a, b) <- liftM (siteToWords _locations) (return i)
  logDebug (printf "Between potential morphs %s and %s" a b)
  let c = a ++ b
  (fullSites', splitSites') <- wordsToSites _startLookup _endLookup a b
  let fullSites = Set.intersection fullSites' ix
      splitSites = Set.intersection splitSites' ix
      sites = Set.union fullSites splitSites
      nSplit = Set.size splitSites
      nFull = Set.size fullSites
      cs' = (subtractCounts c nFull . subtractCounts a nSplit . subtractCounts b nSplit) _counts
      d = (dist cs' _stop a b _alpha (Set.size sites))
  logDebug (printf "%d split, %d unsplit non-conflicting sites" nSplit nFull)
  numPos <- categorical d
  sites' <- shuffleM (Set.toList sites)
  let (pos, neg) = splitAt numPos sites'
      pos' = Set.fromList pos
      neg' = Set.fromList neg
      nPos = length pos
      nNeg = length neg
      cs'' = (addCounts c nNeg . addCounts a nPos . addCounts b nPos) cs'
      cs''' = Map.fromList $ [(k, v) | (k, v) <- Map.toList cs'', v /= 0]
      locations' = [Location {_value=_value, _morphFinal=if _offset `Set.member` pos' then True else if _offset `Set.member` neg' then False else _morphFinal, _static=_static, _offset=_offset} | Location{..} <- _locations]
      (upS, upE) = computeUpdates splitSites fullSites a b
      luS' = clean $ Map.unionWith (Set.\\) _startLookup upS
      luE' = clean $ Map.unionWith (Set.\\) _endLookup upE      
      (upS', upE') = computeUpdates pos' neg' a b
      luS = clean $ Map.unionWith Set.union luS' upS'
      luE = clean $ Map.unionWith Set.union luE' upE'      
  put $ SamplingState cs''' locations' luS luE
  return $ ix Set.\\ sites

-- | Run one sampling iteration
sample :: (MonadIO m, MonadRandom m, (MonadReader Params) m, MonadState (SamplingState Char) m, MonadLog (WithSeverity String) m) => Int -> m ()
sample i = do
  logInfo (printf "Iteration #%d" i)
  state <- get
  params <- ask
  let indices = Set.fromList [i | (l, i) <- zip (_locations state) [0..], _static l == False]
  iterateUntilM (\s -> Set.size s == 0) sampleSite indices
  return ()

--
--   Top-level actions
--

-- | Train a model on given data
train :: (MonadIO m, MonadLog (WithSeverity String) m) => [Char] -> Params -> Int -> StdGen -> m (SamplingState Char)
train seq params iterations gen = do
  logNotice "Training model..."
  let (locations, gen') = randomizeLocations (createData params seq) gen
      counts = initializeCounts locations
      (lookupS, lookupE) = initializeLookups locations
      state = SamplingState counts locations lookupS lookupE
  runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen') state) params

-- | Apply the model to given data
apply :: (MonadIO m, MonadLog (WithSeverity String) m) => Params -> Locations Char -> [Char] -> Int -> StdGen -> m (SamplingState Char)
apply params model sequence iterations gen = do
  logNotice "Applying model..."
  let locations' = model ++ (createData params sequence)
      locations = [l { _offset=i } | (i, l) <- zip [0..] locations']
      counts = initializeCounts locations
      (lookupS, lookupE) = initializeLookups locations
      state = SamplingState counts locations lookupS lookupE
  liftIO $ print $ sort [(v, k) | (k, v) <- Map.toList counts]
  runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen) state) params

--
--   Main entrypoint
--

main :: IO ()
main = do
  args <- unwrapRecord "Type-based sampling for morphological models with Dirichlet process prior on words"
  gen <- case randomSeed args of Nothing -> getStdGen
                                 Just i -> return $ mkStdGen i
  let level = logLevels Map.! (fromMaybe "info" (logLevel args))
  runLoggingT (case args of
                  Train{..} -> do
                    seq <- liftIO $ (liftM (concat . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) input
                    let params = Params (fromMaybe 0.1 alphaParam) (fromMaybe 0.5 sharpParam) useSpaces typeBased
                    state <- train seq params iterations gen
                    liftIO $ writeFile model (show $ (params, _locations state))
                  Apply{..} -> do
                    seq <- liftIO $ (liftM (concat . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) input
                    (params :: Params, modelLocations :: Locations Char) <- (liftIO $ (liftM read . readFile) model)
                    ls <- apply params [x {_static=True } | x <- modelLocations] seq iterations gen
                    liftIO $ writeFile labeled (show ls)) (\msg -> case msgSeverity msg <= level of
                                                              True -> putStrLn (discardSeverity msg)
                                                              False -> return ()
                                                          )
