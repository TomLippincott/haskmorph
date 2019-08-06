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
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..), Model(..))
import Text.HaskSeg.Metrics (f1)
import Text.HaskSeg.Utils (readState, readDataset, writeDataset, writeState, datasetToVocabulary, applySegmentation)
import Text.HaskSeg.Location (randomFlip, createData, randomizeLocations, updateLocations, nonConflicting, wordsToSites, siteToWords, updateLocations', formatWord, showLexicon, initReverseLookup)
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
                          , sharpParam :: w ::: Maybe Double <?> "Probability to stop generating characters when drawing an unseen word (default: 0.5)"
                          , etaParam :: w ::: Maybe Double <?> "Initial probability of each site being a boundary (default: 1.0)"
                          , useSpaces :: w ::: Bool <?> "Make whitespace characters static borders (default: false)"
                          , typeBased :: w ::: Bool <?> "Run over word types, rather than tokens (default: false)"
                          , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                          , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                          , minCount :: w ::: Maybe Int <?> "Only consider words with the given minimum frequency"                          
                          }
                  | Segment { inputFile :: w ::: Maybe String <?> "Input data file"
                            , lineCount :: w ::: Maybe Int <?> "Number of lines to read (default: all)"
                            , stateFile :: w ::: Maybe String <?> "Sampling state file"
                            , segmentationFile :: w ::: Maybe String <?> "Output file for segmented text"
                            , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                            , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                            }
  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

instance (MonadLog (WithSeverity String) m) => MonadLog (WithSeverity String) (RandT g m)

-- --
-- --   Top-level actions
-- --
-- -- | Train a model on given data
trainModel :: (Categorical p, Show p, Probability p, MonadIO m, MonadLog (WithSeverity String) m) => Vector Char -> Double -> (Params p) -> Int -> StdGen -> m (SamplingState Char)
trainModel seq eta params iterations gen = do
  logInfo (printf "Initial random seed: %v" (show gen))
  (locations, gold) <- createData params seq

  let numChars = (length . nub . map (\x -> _value x) . Vector.toList) locations
      charProb = fromDouble $ 1.0 / (fromIntegral numChars)
      (locations', gen') = randomizeLocations eta locations gen
      counts = initializeCounts locations'
      (lookupS, lookupE) = initializeLookups locations'
  --logInfo (show (lookupS, lookupE))
  let rLookup = initReverseLookup lookupS lookupE
  --logInfo (show rLookup)
  let state = SamplingState counts locations' lookupS lookupE rLookup Set.empty
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
                    ds <- liftIO $ readDataset inputFile lineCount
                    let vocab = datasetToVocabulary ds
                        seq = (Vector.fromList . intercalate " ") (Set.toList vocab)
                    --seq <- liftIO $ (liftM (Vector.fromList . intercalate " " . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) inputFile
                    let numChars = (length . nub . Vector.toList) seq
                        charProb = fromDouble $ 1.0 / (fromIntegral numChars) :: ProbRep
                        params = Params (fromDouble $ fromMaybe 0.1 alphaParam) (fromDouble $ fromMaybe 0.5 sharpParam) (fromDouble $ 1.0 - (fromMaybe 0.5 sharpParam)) useSpaces typeBased Set.empty charProb (fromMaybe 1 minCount)
                    state <- trainModel seq (fromMaybe 1.0 etaParam) params iterations gen
                    liftIO $ writeState stateFile params (_locations state) 
                  Segment{..} -> do
                    dataSet <- liftIO $ readDataset inputFile lineCount
                    (params :: Params ProbRep, locs :: Locations Char) <- liftIO $ readState stateFile
                    let cs = nub $ concat (map concat dataSet)
                    model <- fromState (params, locs) (Just cs)
                    dataSet' <- applyModel model dataSet
                    liftIO $ writeDataset segmentationFile dataSet'

                    --let vocab = datasetToVocabulary ds
                    --seq = (Vector.fromList . intercalate " ") (Set.toList vocab)
                    --liftIO $ print seq
              )
                    (\msg -> case msgSeverity msg <= level of
                               True -> hPutStrLn stderr (discardSeverity msg)
                               False -> return ()
                      )
                    
                    --(params :: (Params ProbRep), modelLocations :: Locations Char) <- liftIO (readState modelFile)
                    --model <- createModel params modelLocations                    
                    --let seg = applyModel params modelLocations vocab
                    --liftIO $ print seg
                    --liftIO $ writeFile segmentationFile (show seg)
                    --ds' = applySegmentation seg ds
                    --liftIO $ writeDataset segmentationFile ds'
