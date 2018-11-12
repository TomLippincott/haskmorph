{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Text.HaskSeg.Utils (readDataset, writeDataset, readModel, writeModel, readVocabulary, writeVocabulary) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Control.Monad (join, liftM, foldM)

import Codec.Compression.GZip (compress, decompress)
import Text.HaskSeg.Types (Locations, Morph, Counts, Site, Location(..), Lookup, showLookup, showCounts, SamplingState(..), Params(..))

type Token = String
type Sentence = [Token]
type Dataset = [Sentence]
type Filename = String
type Model a p = (Params p, Locations a)

readDataset :: Filename -> IO Dataset
readDataset f = undefined

writeDataset :: Dataset -> Filename -> IO ()
writeDataset d f = undefined

readVocabulary :: Filename -> IO Dataset
readVocabulary f = undefined

writeVocabulary :: Dataset -> Filename -> IO ()
writeVocabulary d f = undefined

readModel :: (Read a, Read p) => Filename -> IO (Model a p)
readModel f = (liftM read . liftM T.unpack . liftM T.decodeUtf8 . liftM decompress . BS.readFile) f

writeModel :: (Show a, Show p) => Model a p -> Filename -> IO ()
writeModel (p, l) f = BS.writeFile f ((compress . T.encodeUtf8 . T.pack . show) $ (p, l))
