{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

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
import Text.HaskSeg.Location (randomFlip, createData, randomizeLocations, updateLocations, nonConflicting, wordsToSites, siteToWords, updateLocations', formatWord, showLexicon)
import Text.HaskSeg.Lookup (cleanLookup, initializeLookups, computeUpdates)
import Text.HaskSeg.Counts (cleanCounts, initializeCounts, updateCounts, addCounts, subtractCounts)
import Text.HaskSeg.Logging (showFullState)
import Text.HaskSeg.Model (combinations, oneWordProb, g, distribution, sampleSite, sample)

import Data.Char (toLower, isPunctuation)
import Codec.Compression.GZip (compress, decompress)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T


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

data Parameters w = Train { inputFile :: w ::: String <?> "Input data file"
                          , modelFile :: w ::: String <?> "Model file"
                          , iterations :: w ::: Int <?> "Number of sampling iterations"
                          , lineCount :: w ::: Maybe Int <?> "Number of lines to read (default: all)"
                          , alphaParam :: w ::: Maybe Double <?> "Per-decision concentration parameter (default: 0.1)"
                          , sharpParam :: w ::: Maybe Double <?> "Probability to stop generating characters when drawing an unseen word (default: 0.5)"
                          , etaParam :: w ::: Maybe Double <?> "Initial probability of each site being a boundary (default: 1.0)"
                          , useSpaces :: w ::: Bool <?> "Make whitespace characters static borders (default: false)"
                          , typeBased :: w ::: Bool <?> "Run over word types, rather than tokens (default: false)"
                          , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                          , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                          , minCount :: w ::: Maybe Int <?> "Only consider words with the given minimum frequency"                          
                          }
                  | Apply { inputFile :: w ::: String <?> "Input data file"
                          , modelFile :: w ::: String <?> "Model file"
                          , iterations :: w ::: Int <?> "Number of sampling iterations"
                          , lineCount :: w ::: Maybe Int <?> "Number of lines to read (default: all)"
                          , outputFile :: w ::: String <?> "Output file for labeled data"
                          , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                          , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"
                          }
                  | Inspect { modelFile :: w ::: String <?> "Model file"
                            , randomSeed :: w ::: Maybe Int <?> "Set a deterministic random seed (default: use system RNG)"                          
                            , logLevel :: w ::: Maybe String <?> "Minimum log severity to display, one of [debug, info, warn, error] (default: info)"
                            }

  deriving (Generic)                              

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)


-- | The application monad that handles the RNG, sampling state, parameters, and logging
--type Sampler elem = RandT StdGen (StateT (SamplingState elem) (ReaderT (Params ProbRep) (LoggingT (WithSeverity String) IO)))
instance (MonadLog (WithSeverity String) m) => MonadLog (WithSeverity String) (RandT g m)


--
--   Top-level actions
--

-- | Train a model on given data
train :: (MonadIO m, MonadLog (WithSeverity String) m) => Vector Char -> Double -> (Params ProbRep) -> Int -> StdGen -> m (SamplingState Char)
train seq eta params iterations gen = do
  logInfo (printf "Initial random seed: %v" (show gen))
  (locations, gold) <- createData params seq  
  let numChars = (length . nub . map (\x -> _value x) . Vector.toList) locations
      charProb = fromDouble $ 1.0 / (fromIntegral numChars)
      (locations', gen') = randomizeLocations eta locations gen
      counts = initializeCounts locations'
      (lookupS, lookupE) = initializeLookups locations'
      state = SamplingState counts locations' lookupS lookupE Set.empty
      params' = params { _gold=gold, _charProb=charProb }
  --logNotice (printf "Training model on sequence of length %d" (Vector.length locations'))
  --logNotice (printf "Training model on initial sequence of length %d, final length %d" (Vector.length seq) (Vector.length locations'))  
  runReaderT (execStateT (evalRandT (forM_ [1..iterations] sample) gen') state) params'


--
--   Main entrypoint
--

main :: IO ()
main = do
  args <- unwrapRecord "Type-based sampling for segmentation model with Dirichlet process prior on words"
  gen <- case randomSeed args of Nothing -> getStdGen
                                 Just i -> return $! mkStdGen i
  let level = logLevels Map.! (fromMaybe "info" (logLevel args))

  runLoggingT (case args of
                  Train{..} -> do
                    seq' <- liftIO $ (liftM (Vector.fromList . intercalate " " . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) inputFile
                    let seq = seq' -- (Vector.map toLower . Vector.filter (\c -> not (isPunctuation c))) seq'
                        numChars = (length . nub . Vector.toList) seq
                        charProb = fromDouble $ 1.0 / (fromIntegral numChars)
                        params = Params (fromDouble $ fromMaybe 0.1 alphaParam) (fromDouble $ fromMaybe 0.5 sharpParam) (fromDouble $ 1.0 - (fromMaybe 0.5 sharpParam)) useSpaces typeBased Set.empty charProb (fromMaybe 1 minCount)
                    state <- train seq (fromMaybe 1.0 etaParam) params iterations gen
                    liftIO $ writeModel (params, _locations state) modelFile
                      --BS.writeFile model ((compress . T.encodeUtf8 . T.pack . show) $ (params, _locations state))
                  Inspect{..} -> do
                    (params :: (Params ProbRep), modelLocations :: Locations Char) <- liftIO (readModel modelFile)
                      --(liftIO $ (liftM read . liftM T.unpack . liftM T.decodeUtf8 . liftM decompress . BS.readFile) modelFile)
                    liftIO $ putStrLn (intercalate "\n" (showLexicon modelLocations))
                  Apply{..} -> do
                    --seq <- liftIO $ (liftM (Vector.fromList . concat . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) input
                    --seq <- liftIO $ (liftM (concat . (case lineCount of Nothing -> id; Just lc -> take lc) . lines) . readFile) input
                    lines <- liftIO $ (liftM lines . readFile) inputFile
                    (params :: (Params ProbRep), modelLocations :: Locations Char) <- liftIO (readModel modelFile)
                      --(liftIO $ (liftM read . liftM T.unpack . liftM T.decodeUtf8 . liftM decompress . BS.readFile) modelFile)
                    --let seq' = applyModel (params, modelLocations) (Vector.toList seq)
                    --let lu = toLookup modelLocations
                    liftIO $ print 10
                    --liftIO $ print lu
              )
                    --ls <- apply params (Vector.fromList [x | x <- Vector.toList modelLocations]) seq iterations gen
                    --ls <- apply params (Vector.fromList [x {_static=True } | x <- Vector.toList modelLocations]) seq iterations gen                    
                    --liftIO $ writeFile labeled (show ls))
                    (\msg -> case msgSeverity msg <= level of
                               True -> putStrLn (discardSeverity msg)
                               False -> return $! ()
                      )

  --A.setSGR [A.Reset]
