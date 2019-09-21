{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Utils (readDataset, writeDataset, writeState, readState, datasetToVocabulary, applySegmentation, writeFileOrStdout, readFileOrStdin) where

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
import Data.List (nub, unfoldr, isPrefixOf)
import Data.Maybe (fromMaybe)

import Codec.Compression.GZip (compress, decompress)
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..), Model, Token, Sentence, Dataset)
import Text.HaskSeg.Probability (Probability)

type Filename = String
type Vocabulary = [Token]
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


lineToChars :: String -> Text -> [(Char, Bool)]
lineToChars g t = t''
  where
    t' = unpack t
    lineToChars' :: String -> Maybe ((Char, Bool), [Char])
    lineToChars' [] = Nothing
    lineToChars' (v:i) = Just ((v, g'), i')
      where
        g' = (g /= "" && g `isPrefixOf` i) || (i == [])
        i' = if g' then drop (length g) i else i        
    t'' = unfoldr lineToChars' t'

lineToLocations :: String -> Text -> [Location Char]
lineToLocations g t = t''
  where
    t' = unpack t
    lineToChars' :: String -> Maybe (Location Char, [Char])
    lineToChars' [] = Nothing
    lineToChars' (v:[]) = Just (Location v True True True, [])
    lineToChars' (v:i) = Just (Location v False False g', i')
      where
        g' = (g /= "" && g `isPrefixOf` i) || (i == [])
        i' = if g' then drop (length g) i else i        
    t'' = unfoldr lineToChars' t'


readDataset :: Maybe Filename -> Maybe Int -> Maybe String -> IO [[Location Char]]
readDataset mf n g = do
  bs <- case mf of Just f -> readFile f
                   Nothing -> getContents                   
  --let ls = (map words . (case n of Nothing -> id; Just i -> take i) . lines) bs
  let ls = lines bs  
      g' = fromMaybe "" g
  --let ls = (map words . (case n of Nothing -> id; Just i -> take i) . lines . T.unpack . T.decodeUtf8) bs
  return $ map (lineToLocations g') ls

  
datasetToVocabulary :: Dataset -> Vocabulary
datasetToVocabulary = undefined
--datasetToVocabulary ss = nub ws
--  where
--    ws = concat ss

writeDataset :: Maybe Filename -> Dataset -> IO ()
writeDataset = undefined
-- writeDataset mf cs = case mf of Just f -> BS.writeFile f bs
--                                 Nothing -> BS.hPut stdout bs
--   where
--     bs = (T.encodeUtf8 . T.pack . unlines . map unwords) cs

applySegmentation :: Segmentation -> Dataset -> Dataset
applySegmentation = undefined
--applySegmentation seg ds = map (concat . (map (\w -> Map.findWithDefault [[c] | c <- w] w seg))) ds


--readVocabulary :: Filename -> IO Dataset
--readVocabulary f = undefined

--writeVocabulary :: Filename -> Dataset -> IO ()
--writeVocabulary f d = undefined

writeState :: (Show a, Show p) => Maybe Filename -> Params p -> Locations a -> IO ()
writeState (Just f) p l = BS.writeFile f ((compress . T.encodeUtf8 . T.pack . show) $ (p, l))

readState :: (Read a, Read p) => Maybe Filename -> IO (Params p, Locations a)
readState (Just f) = (liftM (read . T.unpack . T.decodeUtf8 . decompress) . BS.readFile) f
