{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.HaskSeg.Probability (Prob, LogProb, showDist, Probability(..), sampleCategorical) where

import Data.List (unfoldr, nub, mapAccumL, intercalate, sort)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad.Random
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)

newtype Prob = Prob Double deriving (Show, Read, Eq, Ord, Num)
type Dist = Vector Prob
newtype LogProb = LogProb Double deriving (Show, Read, Eq, Ord)
type LogDist = Vector LogProb

showDist :: (Probability p, Show p) => Vector p -> String
showDist ps = intercalate ", " $ (map (\v -> printf "%.8f" v :: String) . map (/ total)) ps'
  where
    ps' = map toDouble (Vector.toList ps)
    total = sum ps'

instance Num LogProb where
  (LogProb a) + (LogProb b) = LogProb (l + (logBase 2 v))
    where
      (l, s) = if a > b then (a, b) else (b, a)
      d = s - l
      v = 1 + (2 ** d)
  (LogProb a) * (LogProb b) = LogProb (a + b)
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger i = fromDouble (fromIntegral i)

instance Fractional Prob where
  recip (Prob a) = Prob (1.0 / a)
  fromRational a = undefined
  
instance Fractional LogProb where  
  recip (LogProb a) = LogProb (-a)
  fromRational a = undefined
  
class (Ord p, Num p, Fractional p) => Probability p where
  fromDouble :: Double -> p
  toDouble :: p -> Double
  unwrap :: p -> Double
  
class (Probability p) => Categorical p where
  sampleCategorical :: (MonadRandom m) => Vector p -> m Int
  sampleCategorical xps = do
    let sums = Vector.scanl (+) (fromDouble 0.0 :: p) xps
        maxP = toDouble $ Vector.last sums
    v <- getRandomR (0.0, maxP)
    let v' = fromDouble v :: p
    return (Vector.length (Vector.takeWhile (\x -> x < v') sums) - 1)

instance Probability LogProb where
  fromDouble p = LogProb (logBase 2 p)
  toDouble (LogProb lp) = 2 ** lp
  unwrap (LogProb lp) = lp
  
instance Probability Prob where
  fromDouble p = Prob p
  toDouble (Prob p) = p
  unwrap (Prob p) = p

instance Categorical LogProb
instance Categorical Prob
