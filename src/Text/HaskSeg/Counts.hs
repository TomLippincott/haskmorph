{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Counts (cleanCounts, initializeCounts, updateCounts, addCounts, subtractCounts) where

import Control.Monad.Random
import Data.Set (Set)
import qualified Data.Set as Set
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
addCounts :: (Ord elem) => Morph elem -> Int -> Counts elem -> Counts elem
addCounts = updateCounts (+)

-- | Convenience function for subtracting counts
subtractCounts :: (Ord elem) => Morph elem -> Int -> Counts elem -> Counts elem
subtractCounts = updateCounts (flip (-))
