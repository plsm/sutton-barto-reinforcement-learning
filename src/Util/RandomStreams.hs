-- |
-- Module      : Util.RandomStreams
-- Description : This module provides a set of functions that return a stream of random numbers (either integers or reals) or a stream of coin flips.
-- Copyright   : (c) Pedro Mariano, 2022
-- License     : GPL-3
-- Maintainer  : plsmo@iscte-iul.pt
-- Stability   : stable
-- Portability : POSIX
--
-- This module provides a set of functions that return a stream of
-- random numbers (either integers or reals) or a stream of coin
-- flips.
--

-- History
--
-- 2022-11-25   First version


module Util.RandomStreams (
  streamRandomPositiveInts,
  streamRandomFloats,
  streamRandomFlips
  ) where

import System.Random
import Data.Convertible
import Data.Ratio

-- | Return a list of random positive integers.  Maximum returned value is
-- the maximum integer value minus one.
streamRandomPositiveInts ::
     Int    -- ^ Seed of the pseudo random number generator.
  -> [Int]
streamRandomPositiveInts seed =
  let
    rng = mkStdGen seed
    from = 0 :: Int
    to = maxBound - 1 :: Int
  in
    randomRs (from, to) rng

-- | Return a list of random floating point numbers from the interval [0,1[.
streamRandomFloats ::
     Int        -- ^ Seed of the pseudo random number generator.
  -> [Float]
streamRandomFloats seed =
  let
    rng = mkStdGen seed
    from = 0 :: Int
    to = maxBound - 1 :: Int
    divisor = convert (to + 1) :: Integer
    cvt = (\n -> convert ((%) (convert n) divisor))
  in
    map cvt (randomRs (from, to) rng)

-- | Return a list of random boolean values.  The probability of getting true can be set.
--
-- This function can be used to simulate a biased coin toss.
streamRandomFlips ::
     Int        -- ^ Seed of the pseudo random number generator.
  -> Float      -- ^ Probability of getting True.
  -> [Bool]
streamRandomFlips seed successChance =
  let
    rng = mkStdGen seed
    from = 0 :: Int
    to = maxBound - 1 :: Int
    cut' = successChance * convert (maxBound :: Int)
    cut = convert cut'
    cvt n = n < cut
  in
    map cvt (randomRs (from, to) rng)
