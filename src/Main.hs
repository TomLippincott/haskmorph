{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude hiding (lookup)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM, foldM)
import System.IO (withFile, hPutStr, IOMode(..), readFile)
import System.Random (random, randomR, getStdGen, randomIO, randomRIO, getStdRandom, newStdGen, RandomGen)
import Data.List (unfoldr, nub)
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


data Parameters w = Train { input :: w ::: String <?> "Input file"
                          , lineCount :: w ::: Int <?> "Number of lines to read"
                          , iterations :: w ::: Int <?> "Number of sampling iterations"                          
                          , alpha :: w ::: Double <?> "Per-decision concentration parameter (0.1)"
                          --, mu :: w ::: Double <?> "Base distribution"
                          --, alphaRO :: w ::: Double <?> "Per-outcome concentration parameter (0.1 / |O|)?"
                          }
                  | Apply { modelFile :: w ::: String <?> "Model file (output or input, depending on whether training or testing, respectively)"
                          , n :: w ::: Int <?> "Maximum context size (mutually exclusive with --dev, this option takes precedence)"                          
                          , test :: w ::: String <?> "Test file"
                          , scoresFile :: w ::: String <?> "Output file for scores"
                          }
  deriving (Generic)                              


instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)


data Location a = Location { value :: a
                           , morphFinal :: Bool
                           , static :: Bool
                           , offset :: Int
                           } deriving (Show)


type Counts a = Map [a] Int

type Lookup a = Map [a] (Set Int)

type State a = [Location a]
--type Sequence a = Vector a
--type Boundaries = Vector Bool
--type Spaces = Vector Bool


showLocations :: [Location Char] -> String
showLocations ls = concat toks
  where
    toks = map (\x -> if morphFinal x && static x then [value x, '+', ' '] else if morphFinal x then [value x, '|', ' '] else [value x, ' ', ' ']) ls


-- data Site a = Site { before :: [a]
--                    , after :: [a]
--                    , boundary :: Bool
--                    } deriving (Show, Eq, Ord)


-- comp :: (Eq a) => Site a -> Site a -> Bool
-- comp x y = x' == y'
--   where
--     x' = (before x) ++ (after x)
--     y' = (before y) ++ (after y)    


sequenceToLocations :: [a] -> [Location a]
sequenceToLocations xs = nonFinal ++ [final]
  where
    xs' = init xs
    nonFinal = map (\x -> Location x False False (-1)) xs'
    x = last xs
    final = Location x True True (-1)


-- | Switch each potential morpheme boundary (i.e. intra-word indices) to
--   True or False, uniformly at random
randomizeLocations :: [Location a] -> IO [Location a]
randomizeLocations xs = do
  g <- newStdGen
  bs <- sequence $ [if static == True then return True else randomIO :: IO Bool | Location{..} <- xs]
  let xs' = [x { morphFinal=b } | (x, b) <- zip xs bs]
  return xs'


-- oneSegmentation :: [Location a] -> Maybe ([a], [Location a])
-- oneSegmentation [] = Nothing
-- oneSegmentation xs = Just (morph, rest)
--   where
--     (pref, f:rest) = span (\i -> static i == False) xs
--     morph = map value (pref ++ [f])


--segmentations :: (Show a) => [Location a] -> [[a]]
--segmentations = unfoldr oneSegmentation


-- indexToSite :: (Show a) => [Location a] -> Int -> Site a
-- indexToSite ls i = site
--   where
--     (before, after) = splitAt (i + 1) ls
--     (loc:before') = reverse before
--     b = reverse $ takeWhile (\l -> morphFinal l == False) before'    
--     (a, f:_) = span (\l -> morphFinal l == False) after
--     before'' = map value (b ++ [loc])
--     after'' = map value (a ++ [f])
--     site = Site before'' after'' (morphFinal loc)
--     --(map value (b ++ [loc] ++ a ++ [f])) (length b)


-- randomPivot :: (Show a) => [Location a] -> [Int] -> IO (Int, Site a)
-- randomPivot ls is = do
--   g <- newStdGen
--   let (i, _) = randomR (0, length is) g
--   return (i, indexToSite ls i)


-- pivotToSet :: (Show a, Eq a) => [Location a] -> Site a -> ([Int], [Int])
-- pivotToSet ls s = (pos, neg)
--   where
--     pivots = map (\i -> (i, indexToSite ls i)) [0..length ls - 2]
--     keep = map fst (filter (\(i, s') -> s `comp` s') pivots)
--     pos = [i | i <- keep, (morphFinal $ ls !! i) == True]
--     neg = [i | i <- keep, (morphFinal $ ls !! i) == False]




-- | Initialize word counts from scratch, given sampling state
initializeCounts :: (Ord a, Show a) => State a -> Counts a
initializeCounts ls = Map.fromListWith (+) (map (\x -> (x, 1)) words')
  where
    words = unfoldr (\xs -> case span (\x -> morphFinal x == False) xs of
                              ([], []) -> Nothing
                              (xs', x:ys) -> Just (xs' ++ [x], ys)
                    ) ls
    words' = map (map value) words


-- | Initialize word lookup from scratch, given sampling state
initializeLookups :: (Ord a, Show a) => State a -> (Lookup a, Lookup a)
initializeLookups ls = go ls Map.empty Map.empty []
  where
    go (l:ls') mS mE w = case morphFinal l of
                       False -> go ls' mS mE w'
                       True -> go ls' (Map.insertWith (Set.union) (reverse w') (Set.singleton $ offset l - (length w + 1)) mS) (Map.insertWith (Set.union) (reverse w') (Set.singleton $ offset l) mE) []
      where
        w' = value l : w
    go [] mS mE w = (mS, mE)


      --concat $ [if boundary s == True then [before s, after s] else [before s ++ after s] | s <- ss]
-- --[(indexToSite ls i, 1) | i <- [0..length ls - 2]]


--modifyCounts :: (Ord a, Show a) => (Int -> Int -> Int) -> Counts a -> Int -> Int -> [a] -> [a] -> Counts a
--modifyCounts f cs fs ss a b = cs
--   where
--     b = before s
--     a = after s
--     ba = b ++ a
--     cs' = Map.insertWith f b p cs
--     cs'' = Map.insertWith f a p cs'
--     cs''' = Map.insertWith f ba n cs''


categorical :: [Double] -> IO Int
categorical ps = do
  let s = sum ps
      ps' = map (\x -> x / s) ps
      ps'' = scanl (+) 0.0 ps'
  v <- randomRIO (0.0, 1.0 :: Double)
  return $ length (takeWhile (\x -> x < v) ps'') - 1


siteToSet :: State a -> Lookup a -> Int -> [Int]
siteToSet st lu i = []

choosePivot :: (RandomGen g) => [Int] -> g -> (Int, g)
choosePivot ix = error "dsa"

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f [x] = if f x then [x] else error "Final item not a boundary!"
takeUntil f (x:xs) = if f x then [x] else (x:takeUntil f xs)

siteToWords :: State a -> Int -> ([a], [a])
siteToWords st s = (map value a, map value b)
  where    
    after = drop (s + 1) st
    b = takeUntil (\x -> morphFinal x) after
    (i:before) = reverse (take (s + 1) st)
    a = drop 1 $ reverse $ i : (takeUntil (\x -> morphFinal x) before)
    
wordsToSites :: (Ord a) => Lookup a -> Lookup a -> [a] -> [a] -> ([Int], [Int])
wordsToSites luS luE a b = (jS, splits)
  where
    j = a ++ b
    jS = map (\x -> x + (length a)) (Set.toList $ Map.findWithDefault Set.empty j luS)
    aE = Map.findWithDefault Set.empty a luE
    bS = Map.findWithDefault Set.empty b luS
    splits = Set.toList $ Set.intersection aE bS
    
sample :: [Int] -> (Counts Char, Lookup Char, Lookup Char, State Char) -> Int -> IO (Counts Char, Lookup Char, Lookup Char, State Char)
sample ix (cs, luS, luE, st) iter = do
  print luS
  print luE
  printf "Current state: %s\n" $ showLocations st
  printf "Sites        : %s\n" $ concat [(if i < 10 then " " else "") ++ (show i) ++ " "| i <- [0..length st - 1]]  
  g <- newStdGen
  let (i, g') = (\(a, b) -> (ix !! a, b)) $ randomR (0, length ix - 1) g
  printf "Chose pivot: %d\n" i
  let (a, b) = siteToWords st i
      c = a ++ b
  printf "Corresponding words: %s, %s\n" a b
  let (fullSites, splitSites) = wordsToSites luS luE a b
      sites = fullSites ++ splitSites
      cs' = Map.insertWith (-) c (length fullSites) cs
      cs'' = Map.insertWith (-) a (length splitSites) cs'
      cs''' = Map.insertWith (-) b (length splitSites) cs''
  printf "Matching sites (full/split): %s/%s\n" (show fullSites) (show splitSites)
  let (numPos, g'') = randomR (0, length sites) g'
  print numPos
  let (allIndices, g''') = shuffle (Vector.fromList sites) g''
      (pos, neg) = splitAt numPos (Vector.toList allIndices)
      pos' = Set.fromList pos
      neg' = Set.fromList neg
      cs'''' = Map.insertWith (+) c (length neg) cs'''
      cs''''' = Map.insertWith (+) a (length pos) cs''''
      cs'''''' = Map.insertWith (+) b (length pos) cs'''''
      st' = [Location {value=value, morphFinal=if offset `Set.member` pos' then True else if offset `Set.member` neg' then False else morphFinal, static=static, offset=offset} | Location{..} <- st]
      (luS', luE') = initializeLookups st'
  return (cs'''''', luS', luE', st')

main :: IO ()
main = do
  ps <- unwrapRecord "Type-based sampling for models with Dirichlet process priors"
  words <- (liftM (nub . concat . map words . take (lineCount ps) . lines) . readFile) (input ps)
  let locations' = (Location '#' True True (-1)) : (concat $ map sequenceToLocations words)
      locations'' = [l { offset=i } | (i, l) <- zip [0..] locations']
      indices = [i | (l, i) <- zip locations' [0..], static l == False]
  locations <- randomizeLocations locations''
  let counts = initializeCounts locations
      (lookupS, lookupE) = initializeLookups locations
  (counts', lookupS', lookupE', locations') <- foldM (sample indices) (counts, lookupS, lookupE, locations) [1..iterations ps]
  return ()
