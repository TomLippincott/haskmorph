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
import Control.Monad.State.Strict
import Control.Monad.Random
import System.Random.Shuffle (shuffleM)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

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
type Dist = Vector Double
newtype LogProb = LogProb Double deriving (Show, Read, Eq, Ord, PrintfArg, Fractional)
type LogDist = Vector LogProb

logProb :: Prob -> LogProb
logProb p = LogProb (logBase 2 p)

logDist :: Dist -> LogDist
logDist ps = Vector.map logProb ps

prob :: LogProb -> Prob
prob (LogProb lp) = 2 ** lp

dist :: LogDist -> Dist
dist lps = Vector.map prob lps

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

type Locations elem = Vector (Location elem)
type Morph elem = Vector elem
type Counts elem = Map (Morph elem) Int
type Site = Int

data Location elem = Location { _value :: elem
                              , _morphFinal :: Bool
                              , _static :: Bool
                              } deriving (Show, Read)

-- | A "start" lookup points to the boundary *before* the first item, an "end" lookup points to the boundary *of* the last item
type Lookup elem = Map (Morph elem) (Set Int)

-- | A coherent state of boundary assignments, counts, and word start/end lookups
data SamplingState elem = SamplingState { _counts :: Counts elem
                                        , _locations :: Locations elem
                                        , _startLookup :: Lookup elem
                                        , _endLookup :: Lookup elem
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

getStatic :: (Ord a) => Locations a -> Set [a]
getStatic ls = Set.fromList $ unfoldr (\ls'' -> case span (\x -> _static x == False) ls'' of ([], []) -> Nothing
                                                                                             (w, f:rs) -> Just (map _value (w ++ [f]), rs)
                                      ) ls'
  where
    ls' = Vector.toList ls
    
createData :: Params -> Vector Char -> Locations Char
createData Params{..} cs' = ls
  where
    cs = Vector.toList cs'
    ws = (if _types == True then nub else id) $ if _spaces == True then words cs else [cs]
    ls = Vector.concat [sequenceToLocations w | w <- ws]

randomFlip p g = (v < p, g')
  where
    (v, g') = randomR (0.0, 1.0) g

-- | Switch each potential morpheme boundary (i.e. intra-word indices) to True or False
randomizeLocations :: Double -> Locations elem -> StdGen -> (Locations elem, StdGen)
randomizeLocations p xs g = (Vector.fromList xs', g')
  where
    (g', bs) = mapAccumL (\g'' Location{..} -> if _static == True then (g'', True) else swap (randomFlip p g'' :: (Bool, StdGen))) g (Vector.toList xs)
    xs' = [x { _morphFinal=b } | (x, b) <- zip (Vector.toList xs) bs]

updateLocations :: elem -> Locations elem -> Set Int -> Set Int -> Locations elem
updateLocations a ls pos neg = Vector.update ls updates
  where
    p = Location a True False
    n = Location a False False
    pos' = (Vector.map (\i -> (i, p)) . Vector.fromList . Set.toList) pos
    neg' = (Vector.map (\i -> (i, n)) . Vector.fromList . Set.toList) neg
    updates = pos' Vector.++ neg'
    
-- | Pretty-print a sequence of locations
showLocations :: Locations Char -> [Char]
showLocations ls = unlines [concat toks, concat indices]
  where
    --spacing = 
    toks = Vector.toList $ Vector.map (\x -> if _morphFinal x && _static x then [_value x, '+', ' '] else if _morphFinal x then [_value x, '|', ' '] else [_value x, ' ', ' ']) ls    
    indices = [printf "%3d" i | i <- [0..Vector.length ls]]

-- | Turn a sequence of values into a sequence of locations
sequenceToLocations :: [elem] -> Locations elem
sequenceToLocations xs = Vector.fromList $ nonFinal ++ [final]
  where
    xs' = init xs
    nonFinal = map (\x -> Location x False False) xs'
    x = last xs
    final = Location x True True

-- | Find the two words implied by a boundary at the given site
siteToWords :: (Show elem, MonadLog (WithSeverity String) m) => Locations elem -> Int -> m (Morph elem, Morph elem)
siteToWords ls s = do
  logDebug (printf "Finding words for site %d" s)
  let (before, after) = Vector.splitAt (s + 1) ls
      (bPref, bRem) = Vector.break _morphFinal after
      (b', before') = Vector.splitAt 1 (Vector.reverse before)
      (aPref, aRem) = Vector.break _morphFinal before'
      b = case Vector.length bRem of 0 -> bPref
                                     _ -> bPref Vector.++ (Vector.fromList [Vector.head bRem])
      (before'', after'') = (Vector.map _value (Vector.reverse (b' Vector.++ aPref)), Vector.map _value b)
  logDebug (printf "Found words %s and %s" (show before'') (show after''))
  return (before'', after'')
  

-- | For two words, return all compatible sites
wordsToSites :: (MonadLog (WithSeverity String) m) => (Ord elem) => Lookup elem -> Lookup elem -> Morph elem -> Morph elem -> m (Set Int, Set Int)
wordsToSites luS luE a b = do
  let j = a Vector.++ b
      jS = Vector.fromList $ map (\x -> x + (Vector.length a)) (Set.toList $ Map.findWithDefault Set.empty j luS)
      aE = Map.findWithDefault Set.empty a luE
      bS = Map.findWithDefault Set.empty b luS
      splits = Set.intersection aE bS
  return ((Set.fromList . Vector.toList) jS, splits)

--
--   Sampling-distribution-related functions
--

-- | Sample a value from a log-categorical distribution
categorical :: (MonadRandom m) => LogDist -> m Int
categorical lps = do
  let ps = Vector.scanl (+) (logProb 0.0) lps
      ps' = Vector.map prob ps
  v <- getRandomR (0.0, Vector.last ps' :: Double)
  return (Vector.length (Vector.takeWhile (\x -> x < v) ps') - 1)

-- | Compute the log-probability of generating the given word n times, based on counts
oneWordProb :: (Show elem, Ord elem) => Counts elem -> Double -> Double -> Int -> Morph elem -> LogProb
oneWordProb counts stopProb alpha n word = LogProb (numer / denom)
  where
    mu = ((1.0 - stopProb) ^ (length word)) * stopProb
    total = fromIntegral $ sum $ Map.elems counts
    count = fromIntegral $ Map.findWithDefault 0 word counts
    numer = ((alpha * mu) + count) ^ n
    denom = (alpha + total) ^ n    

-- | Compute the log-probability of setting a single set of m sites, out of n, to positive
g :: (Ord elem, Show elem) => Counts elem -> Double -> Morph elem -> Morph elem -> Double -> Int -> Int -> LogProb
g counts stopProb before after alpha n m= posProb * negProb
  where
    beforeProb = oneWordProb counts stopProb alpha m before
    afterProb = oneWordProb counts stopProb alpha m after
    posProb = beforeProb * afterProb
    negProb = oneWordProb counts stopProb alpha (n - m) (before Vector.++ after)

-- | Compute the log-categorical distribution of possible number of sites to set to positive
distribution :: (Show elem, Ord elem) => Counts elem -> Double -> Morph elem -> Morph elem -> Double -> Int -> LogDist
distribution counts stopProb before after alpha n = Vector.fromList unScaled
  where
    combinations = [(n `choose` m) | m <- [0..n]]
    gs = [g counts stopProb before after alpha n m | m <- [0..n]]
    unScaled = map (\(x, y) -> foldl (+) (logProb 0.0) (replicate x y)) (zip combinations gs)

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
sampleSite :: (MonadIO m, MonadLog (WithSeverity String) m, MonadRandom m, MonadState (SamplingState Char) m, MonadReader Params m) => Set Int -> m (Set Int)
sampleSite ix = do
  Params{..} <- ask
  SamplingState{..} <- get
  --logDebug (printf "%s" (showLocations _locations))
  logDebug (printf "%d sites remaining" (Set.size ix))
  i <- uniform ix
  logDebug (printf "Chose site %d" i)
  (a, b) <- siteToWords _locations i
  let c = a Vector.++ b
  (fullSites', splitSites') <- wordsToSites _startLookup _endLookup a b
  let fullSites = Set.intersection fullSites' ix
      splitSites = Set.intersection splitSites' ix
      sites = Set.union fullSites splitSites
      nSplit = Set.size splitSites
      nFull = Set.size fullSites
      cs' = (subtractCounts c nFull . subtractCounts a nSplit . subtractCounts b nSplit) _counts
      d = (distribution cs' _stop a b _alpha (Set.size sites))
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
      locations' = updateLocations (_value (_locations Vector.! i)) _locations pos' neg'
      (upS, upE) = computeUpdates splitSites fullSites a b
      luS' = Map.unionWith (Set.\\) _startLookup upS
      luE' = Map.unionWith (Set.\\) _endLookup upE      
      (upS', upE') = computeUpdates pos' neg' a b
      luS = Map.unionWith Set.union luS' upS'
      luE = Map.unionWith Set.union luE' upE'      
  put $ SamplingState cs''' locations' luS luE
  return $ ix Set.\\ sites

-- | Run one sampling iteration
sample :: (MonadIO m, MonadRandom m, (MonadReader Params) m, MonadState (SamplingState Char) m, MonadLog (WithSeverity String) m) => Int -> m ()
sample i = do
  logInfo (printf "Iteration #%d" i)
  state@(SamplingState{..}) <- get
  params <- ask
  let indices = Set.fromList [i | (l, i) <- zip ((Vector.toList _locations)) [0..], _static l == False]
  iterateUntilM (\s -> Set.size s == 0) sampleSite indices
  put $ state { _counts=cleanCounts _counts, _startLookup=cleanLookup _startLookup, _endLookup=cleanLookup _endLookup }
  return ()

--
--   Top-level actions
--

-- | Train a model on given data
train :: (MonadIO m, MonadLog (WithSeverity String) m) => Vector Char -> Double -> Params -> Int -> StdGen -> m (SamplingState Char)
train seq eta params iterations gen = do
  let (locations, gen') = randomizeLocations eta (createData params seq) gen
      counts = initializeCounts locations
      (lookupS, lookupE) = initializeLookups locations
      state = SamplingState counts locations lookupS lookupE
  logNotice (printf "Training model on initial sequence of length %d, final length %d, %d types" (Vector.length seq) (Vector.length locations) (Set.size $ getStatic locations))
  runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen') state) params

-- | Apply the model to given data
apply :: (MonadIO m, MonadLog (WithSeverity String) m) => Params -> Locations Char -> Vector Char -> Int -> StdGen -> m (SamplingState Char)
apply params model sequence iterations gen = do
  logNotice "Applying model..."
  let locations = model Vector.++ (createData params sequence)
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
  print gen
  let level = logLevels Map.! (fromMaybe "info" (logLevel args))
  runLoggingT (case args of
                  Train{..} -> do
                    seq <- liftIO $ (liftM (Vector.fromList . concat . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) input
                    let params = Params (fromMaybe 0.1 alphaParam) (fromMaybe 0.5 sharpParam) useSpaces typeBased
                    state <- train seq (fromMaybe 1.0 etaParam) params iterations gen
                    liftIO $ writeFile model (show $ (params, _locations state))
                  Apply{..} -> do
                    seq <- liftIO $ (liftM (Vector.fromList . concat . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) input
                    (params :: Params, modelLocations :: Locations Char) <- (liftIO $ (liftM read . readFile) model)
                    ls <- apply params (Vector.fromList [x {_static=True } | x <- Vector.toList modelLocations]) seq iterations gen
                    liftIO $ writeFile labeled (show ls)) (\msg -> case msgSeverity msg <= level of
                                                              True -> putStrLn (discardSeverity msg)
                                                              False -> return ()
                                                          )
