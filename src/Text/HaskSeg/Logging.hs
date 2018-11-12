{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Logging (showFullState) where

import Prelude hiding (lookup)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM, foldM)
import System.IO (withFile, hPutStr, IOMode(..), readFile)
import System.Random (getStdGen, mkStdGen)
import Data.List (unfoldr, nub, mapAccumL, intercalate, sort, foldl1')
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
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
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified System.Console.ANSI as A
import Text.HaskSeg.Probability (Prob, LogProb, Probability(..), showDist, sampleCategorical)
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..))
import Text.HaskSeg.Metrics (f1)
import Text.HaskSeg.Utils (readModel, writeModel, readDataset, writeDataset, readVocabulary, writeVocabulary)
import Text.HaskSeg.Location (randomFlip, createData, randomizeLocations, updateLocations, nonConflicting, wordsToSites, siteToWords, updateLocations')
import Text.HaskSeg.Lookup (cleanLookup, initializeLookups, computeUpdates)
import Text.HaskSeg.Counts (cleanCounts, initializeCounts, updateCounts, addCounts, subtractCounts)



goldA = A.setSGRCode [A.SetColor A.Background A.Vivid A.Green]
goldB = A.setSGRCode [A.SetColor A.Background A.Dull A.Green]
goldAlts = [if i `mod` 2 == 0 then goldA else goldB | i <- [1..]]

goldFormat = A.setSGRCode [A.SetColor A.Background A.Vivid A.Blue]
staticFormat = A.setSGRCode [A.SetColor A.Background A.Vivid A.Yellow]
sampleFormat = A.setSGRCode [A.SetColor A.Foreground A.Vivid A.Red]
siteFormat = A.setSGRCode [A.SetUnderlining A.SingleUnderline]
pivotFormat = A.setSGRCode [A.SetConsoleIntensity A.BoldIntensity, A.SetUnderlining A.SingleUnderline]

sampleA = A.setSGRCode [A.SetColor A.Foreground A.Vivid A.Black]
sampleB = A.setSGRCode [A.SetColor A.Foreground A.Vivid A.Red]
sampleAlts = [if i `mod` 2 == 0 then sampleA else sampleB | i <- [1..]]

reset = A.setSGRCode [A.Reset]


showFullState :: (Probability p, IsChar elem, MonadState (SamplingState elem) m, MonadReader (Params p) m, PrintfArg elem) => Maybe Int -> Maybe (Set Int) -> m String
showFullState mi ms = do
  SamplingState{..} <- get
  params@(Params{..}) <- ask  
  let ls = (Vector.toList . Vector.indexed) _locations
      renderChar ([], golds, samples) = Nothing
      renderChar ((i, Location{..}):locs, golds, samples) = Just (formatting ++ (printf "%v" _value) ++ reset, (locs, golds', samples'))
        where
          g:gs = golds
          s:ss = samples
          isGold = i `Set.member` _gold
          isSet = _morphFinal
          isPivot = Just i == mi
          isStatic = _static
          isSite = i `Set.member` (fromMaybe Set.empty ms)
          gf = if isGold then Just goldFormat else Nothing
          sf = if isSet then Just sampleFormat else Nothing
          pf = if isPivot then Just pivotFormat else Nothing          
          ssf = if isSite then Just siteFormat else Nothing
          stf = if isStatic then Just staticFormat else Nothing
          formatting = (concat . catMaybes) [gf, sf, pf, ssf, stf]
          golds' = if isGold then gs else g:gs
          samples' = if isSet then ss else s:ss
      toks = unfoldr renderChar (ls, goldAlts, sampleAlts)
  --  return $ concat toks
  return $! (intercalate "\n" [concat toks, printf "Starts: %v" (showLookup _startLookup), printf "Ends: %v" (showLookup _endLookup), printf "Counts: %v" (showCounts _counts)])
