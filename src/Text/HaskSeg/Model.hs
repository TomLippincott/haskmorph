module Text.HaskSeg.Model (applyModel, combinations, oneWordProb, g, distribution, sampleSite, sample, fromState) where

import Data.List (unfoldr, nub, mapAccumL, intercalate, sort, foldl1', sortOn, maximumBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Math.Combinatorics.Exact.Binomial (choose)
import Control.Monad.Loops
import Control.Monad.Log
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Reader.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Random
import System.Random.Shuffle (shuffleM)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.HaskSeg.Probability (Prob, LogProb, Probability(..), showDist, sampleCategorical, Categorical)
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..), Vocabulary, Segmentation, Dataset)
import Text.HaskSeg.Metrics (f1)
import Text.HaskSeg.Location (randomFlip, createData, randomizeLocations, updateLocations, nonConflicting, wordsToSites, siteToWords, updateLocations')
import Text.HaskSeg.Lookup (cleanLookup, initializeLookups, computeUpdates)
import Text.HaskSeg.Counts (cleanCounts, initializeCounts, updateCounts, addCounts, subtractCounts)
import Text.HaskSeg.Probability (Prob, LogProb, Probability(..), showDist, sampleCategorical)
import Debug.Trace (traceShowId)
import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Array.ST


type Model p elem = Map (Vector elem) p


fromState :: (MonadLog (WithSeverity String) m, Ord elem, Show elem, Probability p) => (Params p, Locations elem) -> Maybe [elem] -> m (Model p elem)
fromState (p, ls) cs = do
  let cts = initializeCounts ls
      ups = case cs of Nothing -> Map.fromList []
                       Just cs' -> Map.fromList [(Vector.fromList [c], 1) | c <- cs']
      cts' = Map.unionWith (\a b -> a) cts ups
      ps = map (\w -> (w, oneTimeOneWord cts p w)) (Map.keys cts')      
  return $ Map.fromList ps

    
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
  logInfo (printf "Log-likelihood old/new: %.3v/%.3v\tF-Score old/new: %.3f/%.3f" ll ll' score score')
  return $! ()


formatMorphs :: [Vector Char] -> [Vector Char]
formatMorphs ms = Vector.toList ms'
  where
    suff = Vector.fromList "@@"
    ms' = Vector.imap (\i m -> if i == length ms - 1 then m else Vector.concat [m, suff]) (Vector.fromList ms)


mapAccumLM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM = mapAccumLM' []


mapAccumLM' :: (Monad m) => [c] -> (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM' cs f acc [] = return (acc, reverse cs)
mapAccumLM' cs f acc (b:bs) = do
  (acc', c) <- f acc b  
  mapAccumLM' (c:cs) f acc' bs


applyModel :: (MonadLog (WithSeverity String) m, Probability p, Show p) => Model p Char -> Dataset -> m Dataset
applyModel model dataSet = do
  let uniqueWords = (map Vector.fromList . Set.toList . Set.fromList . concat) dataSet
      segCache = Map.empty :: SegCache p
  logInfo (printf "Segmenting %d words" (length uniqueWords))
  (sc, segs) <- mapAccumLM (segment model) segCache uniqueWords
  let segMap = Map.fromList segs
  return $ map (map Vector.toList . concat . map (\w -> segMap Map.! (Vector.fromList w))) dataSet


type Table p = Map (Int, Int) p
type SegCache p = Map (Vector Char) p


type DPState prob = (SegCache prob, Table prob, Table Int)


traceBack :: (MonadLog (WithSeverity String) m) => Table Int -> Int -> Vector Char -> m [Vector Char]
traceBack pathTable end token = return $ go pathTable end token []
  where
    go pt 0 t acc = acc    
    go pt e t acc = go pt e' t' (s:acc)
      where
        e' = pt Map.! (0, e)
        (t', s) = Vector.splitAt e' t


printTable :: (Show p, Probability p) => Table p -> Int -> String
printTable table size = unlines rows
  where
    rows = map unwords cells
    cells = [[case table Map.!? (r, c)  of Nothing -> "         "
                                           Just p -> printf "%.7f" (toDouble p)
             | c <- [1..size + 1]] | r <- [0..size]]


printPathTable :: Table Int -> Int -> String
printPathTable table size = unlines rows
  where
    rows = map unwords cells
    cells = [[case table Map.!? (r, c)  of Nothing -> "  "
                                           Just p -> printf "%.2d" p
             | c <- [1..size + 1]] | r <- [0..size]]


fillTable :: (MonadLog (WithSeverity String) m, Probability p, Show p) => Model p Char -> Vector Char -> DPState p -> (Int, Int) -> m (DPState p, p)
fillTable model token (cache, probTable, pathTable) (from, to) = do
  --logInfo (printf "Considering span from %d to %d" from to)
  let ct = to - from
      gram = Vector.slice from ct token
      --cachedSeg = cache Map.!? gram
      --noSegProb = model Map.!? gram
      pairs = [(i, Vector.slice (from + i) (to - (from + i)) token) | i <- [0..ct - 1]]
  
  --logInfo (printf "Substring '%s'" (Vector.toList gram))
  --logInfo (show pairs)
  let scores = [(i, (Map.findWithDefault (fromDouble 1.0) (from, from + i) probTable) * (Map.findWithDefault (fromDouble 0.0) g model)) | (i, g) <- pairs]
      --best = (maximumBy (comparing id) . catMaybes) ([noSegProb, Just (fromDouble 0.0)] ++ [])
      (bestI, best) = (maximumBy (comparing snd)) scores --  . catMaybes) ([noSegProb, Just (fromDouble 0.0)] ++ [])
      --case Vector.length gram of 0 -> fromDouble 1.0
      --                                 --_ -> (maximumBy (comparing id)) scores
      --                                 _ -> 
      cache' = Map.insert gram best cache
      probTable' = Map.insert (from, to) best probTable
      pathTable' = Map.insert (from, to) bestI pathTable
  --logInfo (show scores)
  --logInfo (printf "No seg prob for %v: %s" gram (show noSegProb))
  --logInfo (printf "Cache size: %d" (Map.size cache'))
  
  --logInfo (printTable probTable' (Vector.length token - 1))
  --logInfo (printPathTable pathTable' (Vector.length token - 1))
  return ((cache', probTable', pathTable'), best)


segment :: (MonadLog (WithSeverity String) m, Probability p, Show p) => Model p Char -> SegCache p -> Vector Char -> m (SegCache p, (Vector Char, [Vector Char]))
segment model cache token = do
  --logInfo (printf "Segmenting '%s'" (Vector.toList token))
  let max = Vector.length token
      order = concat [[(from, to) | from <- reverse [0..to - 1]] | to <- [1..max]]
      probTable = Map.empty :: Table p
      pathTable = Map.empty :: Table Int
  --logInfo (printf "Sequence of spans to consider: %s" (show order))
  ((cache', probTable', pathTable'), _) <- mapAccumLM (fillTable model token) (cache, probTable, pathTable) order
  --logInfo (printPathTable pathTable' (Vector.length token - 1))


  toks <- traceBack pathTable' max token
  
  return (cache', (token, formatMorphs toks))


splits :: Model p elem -> Vector elem -> [(Vector elem, Vector elem)]
splits m w = [Vector.splitAt i w | i <- [1..Vector.length w]]


segProb :: (Probability p, Ord elem) => Model p elem -> [Vector elem] -> p
segProb m ws = product $ map (\w -> Map.findWithDefault (fromDouble 0.0) w m) ws --fromDouble 1.0


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
  return $! ((numer ^ n) / (denom ^ n))


oneTimeOneWord :: (Probability p, Ord elem) => Counts elem -> Params p -> Vector elem -> p
oneTimeOneWord counts Params{..} word = p
  where
    mu = ((_dontStop * _charProb) ^ (Vector.length word)) * _stop
    total = fromIntegral $ sum $ Map.elems counts
    count = fromIntegral $ Map.findWithDefault 0 word counts
    numer = ((_alpha * mu) + count)
    denom = (_alpha + total)
    p = numer / denom


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
  return $! ix Set.\\ sites

