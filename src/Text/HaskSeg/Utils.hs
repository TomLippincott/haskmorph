{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Utils (readDataset, writeDataset, writeState, readState, datasetToVocabulary, applySegmentation) where

import Prelude hiding (lookup, getContents, readFile, strip, lines, writeFile, words)
import System.IO (withFile, IOMode(..), stdin, stderr, openFile, stdout, hClose, Handle(..))
import Data.Text (Text, strip, lines, stripPrefix, splitOn, pack, unpack, words)
import Data.Text.IO (getContents, readFile, hGetContents, hPutStr, writeFile, hPutStrLn)
--import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Control.Monad (join, liftM, foldM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub)

import Codec.Compression.GZip (compress, decompress)
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..), Model, Token, Sentence, Dataset)
import Text.HaskSeg.Probability (Probability)

--type Token = String
--type Sentence = [Token]
--type Dataset = [Sentence]
type Filename = String
type Vocabulary = Set Token
type Segmentation = Map Token [Token]


readFileOrStdin :: Maybe String -> IO Text
readFileOrStdin (Just f) = case suf of "gz" -> (liftM (pack . BS.unpack . decompress . BS.pack . unpack) . readFile) f
                                       _ -> readFile f
  where
    suf = (reverse . take 2 . reverse) f
readFileOrStdin Nothing = getContents


writeFileOrStdout :: Maybe String -> Text -> IO ()
writeFileOrStdout (Just f) s = case suf of "gz" -> writeFile f ((pack . BS.unpack . compress . BS.pack . unpack) s)
                                           _ -> writeFile f s
  where
    suf = (reverse . take 2 . reverse) f
writeFileOrStdout Nothing s = hPutStr stdout s


readDataset :: Maybe Filename -> Maybe Int -> IO Dataset
readDataset (Just f) n = do
  bs <- readFile f
  let ls = (map words . (case n of Nothing -> id; Just i -> take i) . lines) bs
  --let ls = (map words . (case n of Nothing -> id; Just i -> take i) . lines . T.unpack . T.decodeUtf8) bs
  return $ map (map unpack) ls

datasetToVocabulary :: Dataset -> Vocabulary
datasetToVocabulary ss = Set.fromList $ nub ws
  where
    ws = concat ss

writeDataset :: Maybe Filename -> Dataset -> IO ()
writeDataset (Just f) cs = BS.writeFile f bs
  where
    bs = (T.encodeUtf8 . T.pack . unlines . map unwords) cs

applySegmentation :: Segmentation -> Dataset -> Dataset
applySegmentation seg ds = map (concat . (map (\w -> Map.findWithDefault [[c] | c <- w] w seg))) ds


--readVocabulary :: Filename -> IO Dataset
--readVocabulary f = undefined

--writeVocabulary :: Filename -> Dataset -> IO ()
--writeVocabulary f d = undefined

writeState :: (Show a, Show p) => Maybe Filename -> Params p -> Locations a -> IO ()
writeState (Just f) p l = BS.writeFile f ((compress . T.encodeUtf8 . T.pack . show) $ (p, l))

readState :: (Read a, Read p) => Maybe Filename -> IO (Params p, Locations a)
readState (Just f) = (liftM (read . T.unpack . T.decodeUtf8 . decompress) . BS.readFile) f
