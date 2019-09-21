{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (lookup)
import Data.Text (pack, unpack)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Control.Monad (join, liftM, foldM)
import System.Random (getStdGen, mkStdGen)
import System.IO (stderr, hPutStrLn)
import Data.List (unfoldr, nub, mapAccumL, intercalate, sort, foldl1')
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Monad.Loops
import Control.Monad.Log
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Reader.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Random
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.HaskSeg.Probability (Prob, LogProb, Probability(..), Categorical(..))
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..), Model(..), Character)
import Text.HaskSeg.Metrics (f1)
import Text.HaskSeg.Utils (readState, readDataset, writeDataset, writeState, datasetToVocabulary, applySegmentation, writeFileOrStdout, readFileOrStdin)
import Text.HaskSeg.Location (randomFlip, randomizeLocations, updateLocations, nonConflicting, wordsToSites, siteToWords, updateLocations', formatWord, showLexicon, initReverseLookup)
import Text.HaskSeg.Lookup (cleanLookup, initializeLookups, computeUpdates)
import Text.HaskSeg.Counts (cleanCounts, initializeCounts, updateCounts, addCounts, subtractCounts)
import Text.HaskSeg.Logging (showFullState)
import Text.HaskSeg.Model (combinations, oneWordProb, g, distribution, sampleSite, sample, applyModel, fromState)

type ProbRep = LogProb

--
--   Command-line parsing
--

logLevels :: Map String Severity
logLevels = Map.fromList [ ("debug", Debug)
                         , ("info", Informational)
                         , ("warn", Warning)
                         , ("error", Error)
                         ]

data Parameters w = Train { inputFile :: w ::: Maybe String <?> "Input data file"
                          , lineCount :: w ::: Maybe Int <?> "Number of lines to read (default: all)"
                          , stateFile :: w ::: Maybe String <?> "Sampling state file"
                          , iterations :: w ::: Int <?> "Number of sampling iterations"
                          , alphaParam :: w ::: Maybe Double <?> "Per-decision concentration parameter (default: 0.1)"
                          , sharpParam :: w ::: Maybe Double <?> "Probability to stop generating characters (default: 0.5)"
                          , etaParam :: w ::: Maybe Double <?> "Initial probability of each site being a boundary (default: 1.0)"
                          , typeBased :: w ::: Bool <?> "Run over types (i.e. don't consider frequency) rather than tokens (default: false)"
                          , goldString :: w ::: Maybe String <?> "Treat occurrences of the given string as gold boundaries for evaluation"
                          , logLevel :: w ::: Maybe String <?> "Log severity threshold, one of [debug, info, warn, error] (default: info)"
                          , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                          , minCount :: w ::: Maybe Int <?> "Only consider words with the given minimum frequency"                          
                          }
                  | Segment { inputFile :: w ::: Maybe String <?> "Input data file"
                            , lineCount :: w ::: Maybe Int <?> "Number of lines to read (default: all)"
                            , stateFile :: w ::: Maybe String <?> "Sampling state file"
                            , borderCharacters :: w ::: Maybe String <?> "Characters to consider a static (unsampled) border"
                            , goldString :: w ::: Maybe String <?> "Treat occurrences of the given string as gold boundaries for evaluation"
                            , segmentationFile :: w ::: Maybe String <?> "Output file for segmented text"
                            , logLevel :: w ::: Maybe String <?> "Log severity threshold, one of [debug, info, warn, error] (default: info)"
                            , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                            }
  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

instance (MonadLog (WithSeverity String) m) => MonadLog (WithSeverity String) (RandT g m)

--
--   Top-level actions
--

-- | Train a model on given data
trainModel :: (Categorical p, Show p, Probability p, MonadIO m, MonadLog (WithSeverity String) m) => Vector (Location Char) -> Double -> (Params p) -> Int -> StdGen -> m (SamplingState Char)
trainModel locations eta params iterations gen = do
  logInfo (printf "Initial random seed: %v" (show gen))
  let numChars = (length . nub . map (\x -> _value x) . Vector.toList) locations
      charProb = fromDouble $ 1.0 / (fromIntegral numChars)
      (locations', gen') = randomizeLocations eta locations gen
      counts = initializeCounts locations'
      (lookupS, lookupE) = initializeLookups locations'
      gold = Set.empty
  let rLookup = initReverseLookup lookupS lookupE
      state = SamplingState counts locations' lookupS lookupE rLookup Set.empty
      params' = params { _gold=gold, _charProb=charProb }
  runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen') state) params'


--
--   Main entrypoint
--

main :: IO ()
main = do
  args <- unwrapRecord "Type-based sampling for segmentation model with Dirichlet process prior on words"
  gen <- case randomSeed args of Nothing -> getStdGen
                                 Just i -> return $ mkStdGen i
  let level = logLevels Map.! (fromMaybe "info" (logLevel args))

  runLoggingT (case args of
                  Train{..} -> do
                    dataSet <- liftIO $ readDataset inputFile lineCount goldString
                    let seq = (Vector.fromList . concat) dataSet
                        numChars = (length . nub . map _value . Vector.toList) seq
                        charProb = fromDouble $ 1.0 / (fromIntegral numChars) :: ProbRep
                        params = Params (fromDouble $ fromMaybe 0.1 alphaParam) (fromDouble $ fromMaybe 0.5 sharpParam) (fromDouble $ 1.0 - (fromMaybe 0.5 sharpParam)) False typeBased Set.empty charProb (fromMaybe 1 minCount)
                    state <- trainModel seq (fromMaybe 1.0 etaParam) params iterations gen
                    liftIO $ writeState stateFile params (_locations state)
                    return ()
                  Segment{..} -> do
                    toSeg <- liftIO $ (liftM ((map words) . lines . unpack) . readFileOrStdin) inputFile
                    (params :: Params ProbRep, locs :: Locations Char) <- liftIO $ readState stateFile
                    let cs = nub $ concat (map concat toSeg)
                    model <- fromState (params, locs) (Just cs)
                    segmented <- sequence $ map (applyModel model) toSeg
                    liftIO $ writeFileOrStdout segmentationFile (pack (intercalate "\n" segmented ++ "\n"))
              )
                    (\msg -> case msgSeverity msg <= level of
                               True -> hPutStrLn stderr (discardSeverity msg)
                               False -> return ()
                      )
