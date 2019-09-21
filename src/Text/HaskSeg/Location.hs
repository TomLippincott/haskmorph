{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Location (randomFlip, createData, randomizeLocations, updateLocations, updateLocations', nonConflicting, wordsToSites, siteToWords, formatWord, showLexicon, initReverseLookup) where

import Control.Monad.Random
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Monad.Log
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Reader.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State.Strict
import Data.Tuple (swap)
import Data.List (unfoldr, nub, mapAccumL, intercalate, sort, foldl1')
import Text.HaskSeg.Probability (Prob, LogProb, Probability(..), showDist, sampleCategorical)
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..))
import Debug.Trace (traceShowId)


randomFlip p g = (v < p, g')
  where
    (v, g') = randomR (0.0, 1.0) g


createData :: (Probability p, MonadLog (WithSeverity String) m) => (Params p) -> Vector (Char, Bool) -> m (Locations Char, Set Int)
createData = undefined
-- createData Params{..} cs' = do
--   let cs = (map fst . Vector.toList) cs'
--       ls = lines cs
--       wss = concat $ map words ls
--       wc = Map.fromListWith (\a b -> a + b) (zip wss $ repeat 1)
--       keep = Map.filter (>= _minCount) wc
--       ws = if _types == True then Map.keys keep else concat $ map words ls
--       bs = map length ws
--       bs' = (reverse . drop 1 . reverse . drop 1) $ scanl (+) (-1) bs
--       ws' = if _spaces == True then ws else [concat ws]
--       ws'' = Vector.concat [sequenceToLocations w | w <- ws']
--   logInfo (printf "Loaded data set of %d characters" (length cs)) -- (length ws))
--   return $! (ws'', Set.fromList bs')


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
    pos' = (Vector.map (\i -> (i, (ls Vector.! i) { _morphFinal=True} )) . Vector.fromList . Set.toList) pos
    neg' = (Vector.map (\i -> (i, (ls Vector.! i) { _morphFinal=False} )) . Vector.fromList . Set.toList) neg
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
-- sequenceToLocations :: [elem] -> Locations elem
-- sequenceToLocations xs = Vector.fromList $ nonFinal ++ [final]
--   where
--     xs' = init xs
--     nonFinal = map (\x -> Location x False False) xs'
--     x = last xs
--     final = Location x True True True


-- -- | Find the two words implied by a boundary at the given site
-- siteToWords :: (Show elem, MonadLog (WithSeverity String) m) => Locations elem -> Int -> m (Morph elem, Morph elem)
-- siteToWords ls s = do
--   let (before, after) = Vector.splitAt (s + 1) ls
--       (bPref, bRem) = Vector.break _morphFinal after
--       (b', before') = Vector.splitAt 1 (Vector.reverse before)
--       (aPref, aRem) = Vector.break _morphFinal before'
--       b = case Vector.length bRem of 0 -> bPref
--                                      _ -> bPref Vector.++ (Vector.fromList [Vector.head bRem])
--       (before'', after'') = (Vector.map _value (Vector.reverse (b' Vector.++ aPref)), Vector.map _value b)
--   return $! (before'', after'')

-- initReverseLookup :: Locations elem -> Map Int (Morph elem, Morph elem)
-- initReverseLookup ls = Map.fromList ls'
--   where
--     items = Vector.map _value ls
--     starts = Vector.toList $ Vector.findIndices _morphFinal ls
--     ends = (drop 1 starts) ++ [Vector.length ls]
--     spans = zip starts ends
    
--     dummy = Vector.fromList []
--     ls' = map (\i -> (i, (dummy, dummy))) [0..Vector.length ls]

initReverseLookup :: (Eq elem) => Lookup elem -> Lookup elem -> Map Int (Morph elem, Morph elem)
initReverseLookup s e = Map.fromList [(i, (Maybe.fromJust a, Maybe.fromJust b)) | (i, (a, b)) <- atBoundaries ++ atNonBoundaries, a /= Nothing && b /= Nothing]
  where
    e' = Map.fromList $ concat [[(v', k) | v' <- Set.toList v] | (k, v) <- Map.toList e]
    s' = Map.fromList $ concat [[(v', k) | v' <- Set.toList v] | (k, v) <- Map.toList s]
    indices = Map.keys s'
    atBoundaries = [(i, (e' Map.!? (i), s' Map.!? i)) | i <- indices]
    atNonBoundaries = concat $ [[(i + i', (Just $ Vector.slice 0 i' m, Just $ Vector.slice i' (Vector.length m - i') m)) | i' <- [1..Vector.length m - 1]] | (i, m) <- map (\i -> (i, s' Map.! i)) (Map.keys s')]


-- | Find the two words implied by a boundary at the given site
siteToWords' :: (Show elem, MonadLog (WithSeverity String) m, MonadState (SamplingState elem) m) => Int -> m (Morph elem, Morph elem)
siteToWords' s = do
  SamplingState{..} <- get
  let (a, b) = _wordsLookup Map.! s
  --(a', b') <- siteToWords' s
  --logInfo (show ((a, b), (a', b')))
  
  return (a, b)


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
wordsToSites :: (Probability p, MonadState (SamplingState elem) m, MonadReader (Params p) m, MonadLog (WithSeverity String) m, Show elem, Ord elem, PrintfArg elem) => Int -> Lookup elem -> Lookup elem -> Morph elem -> Morph elem -> m (Set Int, Set Int)
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
