{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}


module Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..)) where

import Data.List (unfoldr, nub, mapAccumL, intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable
  
type Locations elem = Vector (Location elem)
type Morph elem = Vector elem
type Counts elem = Map (Morph elem) Int
type Site = Int

data Location elem = Location { _value :: !elem
                              , _morphFinal :: !Bool
                              , _static :: !Bool
                              } deriving (Show, Read)

-- | A "start" lookup points to the boundary *before* the first item, an "end" lookup points to the boundary *of* the last item
type Lookup elem = Map (Morph elem) (Set Int)

showLookup :: (PrintfArg elem, IsChar elem) => Lookup elem -> String
showLookup lu = intercalate ", " [printf "\"%v\"=[%v]" (toList k) v | (k, v) <- Map.toList lu]

showCounts :: (PrintfArg elem, IsChar elem) => Counts elem -> String
showCounts cs = intercalate ", " [printf "\"%v\"=%d" (toList k) v | (k, v) <- Map.toList cs]

-- | A coherent state of boundary assignments, counts, and word start/end lookups
data SamplingState elem = SamplingState { _counts :: !(Counts elem)
                                        , _locations :: !(Locations elem)
                                        , _startLookup :: !(Lookup elem)
                                        , _endLookup :: !(Lookup elem)
                                        , _toSample :: !(Set Int)
                                        } deriving (Show, Read)

instance Show elem => PrintfArg (SamplingState elem) where
  formatArg SamplingState{..} fmt | fmtChar (vFmt 'P' fmt) == 'P' = formatString (printf "SamplingState" :: String) (fmt { fmtChar = 's', fmtPrecision = Nothing })
  formatArg _ fmt = errorBadFormat $ fmtChar fmt
  
-- | Parameters that are set at training time
data Params p = Params { _alpha :: !p
                       , _stop :: !p
                       , _dontStop :: !p
                       , _spaces :: !Bool
                       , _types :: !Bool
                       , _gold :: !(Set Int)
                       , _charProb :: !p
                       , _minCount :: !Int
                       } deriving (Show, Read)

instance Show p => PrintfArg (Params p) where
  formatArg Params{..} fmt | fmtChar (vFmt 'P' fmt) == 'P' = formatString (printf "Params: alpha=%v, stopProb=%v, dontStop=%v, uniformCharProb=%v" (show _alpha) (show _stop) (show _dontStop) (show _charProb) :: String) (fmt { fmtChar = 's', fmtPrecision = Nothing })
  formatArg _ fmt = errorBadFormat $ fmtChar fmt

instance PrintfArg (Set Int) where
  formatArg is fmt | fmtChar (vFmt 'P' fmt) == 'P' = formatString (intercalate ", " ((map show . Set.toList) is)) (fmt { fmtChar = 's', fmtPrecision = Nothing })
  formatArg _ fmt = errorBadFormat $ fmtChar fmt

instance (Show elem) => PrintfArg (Vector elem) where
  formatArg is fmt | fmtChar (vFmt 'P' fmt) == 'P' = formatString (intercalate ", " ((map show . Vector.toList) is)) (fmt { fmtChar = 's', fmtPrecision = Nothing })
  formatArg _ fmt = errorBadFormat $ fmtChar fmt

