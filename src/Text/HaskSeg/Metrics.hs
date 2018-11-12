{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Metrics (precision, recall, f1) where

import Data.Set (Set)
import qualified Data.Set as Set

precision :: Set Int -> Set Int -> Double
precision guesses golds = tp / gs
  where
    tp = (fromIntegral . Set.size . Set.intersection guesses) golds
    gs = (fromIntegral . Set.size) guesses

recall :: Set Int -> Set Int -> Double
recall guesses golds = tp / gs
  where
    tp = (fromIntegral . Set.size . Set.intersection guesses) golds
    gs = (fromIntegral . Set.size) golds

f1 :: Set Int -> Set Int -> Double
f1 guesses golds = 2.0 * numer / denom
  where
    p = precision guesses golds
    r = recall guesses golds
    numer = p * r
    denom = p + r

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
