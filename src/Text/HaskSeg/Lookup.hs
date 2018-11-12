{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Lookup (cleanLookup, initializeLookups, computeUpdates) where

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
