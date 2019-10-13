{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-
Copied wholesale into this repo with no modifications from the package ease, by Isaac Shapira.
http://hackage.haskell.org/package/ease-0.1.0.1/docs/src/Ease.html
-}

{-|
These are the standard easing equations.
All of the functions in this module expect a fractional between 0 and 1,
and give back a fractional between 0 and 1.
Some eases require some configuration. These options are provided via types
with default instances.
-}

module Ease
  ( Ease
  -- * Back
  , Overshoot(..)
  , backIn
  , backOut
  , backInOut
  -- * Bounce
  , bounceIn
  , bounceOut
  , bounceInOut
  -- * Circ
  , circIn
  , circOut
  , circInOut
  -- * Elastic
  , Amplitude (..)
  , Period (..)
  , elasticIn
  , elasticOut
  , elasticInOut
  -- * Exponential
  , expoIn
  , expoOut
  , expoInOut
  -- * Linear
  , linear
  -- * Quad
  , quadIn
  , quadOut
  , quadInOut
  -- * Cubic
  , cubicIn
  , cubicOut
  , cubicInOut
  -- * Quart
  , quartIn
  , quartOut
  , quartInOut
  -- * Quint
  , quintIn
  , quintOut
  , quintInOut
  -- * Sine
  , sineIn
  , sineOut
  , sineInOut
  -- * Re-export
  , Data.Default.def ) where

import           Data.Default

-- | An Ease is just a function from some fractional to another fractional.
-- Eases expects a term between 0 and 1 and gives a term between 0 and 1.
-- This is not statically checked, but maybe should be.
type Ease a = Fractional a => a -> a


newtype Overshoot a = Overshoot a
instance Fractional a => Default (Overshoot a) where def = Overshoot 1.70158


backIn :: Overshoot a -> Ease a
backIn (Overshoot overshoot) time =
    time * time * ((overshoot + 1) * time - overshoot)


backOut :: Overshoot a -> Ease a
backOut (Overshoot overshoot) (negate -> time) =
    time * time * ((overshoot + 1) * time + overshoot) + 1


backInOut :: Ord a => (Overshoot a) -> Ease a
backInOut (Overshoot ((* 1.525) -> overshoot)) ((* 2) -> time) =
      if time < 1
      then 1 / 2 * (time  * time  * ((overshoot + 1) * time  - overshoot)    )
      else 1 / 2 * (time' * time' * ((overshoot + 1) * time' + overshoot) + 2)
      where time' = time - 2


bounceOut :: Ord a => Ease a
bounceOut time
    | time < 1   / y =                        x * time  * time
    | time < 2   / y = let time' = f 1.5   in x * time' * time' + 0.75
    | time < 2.5 / y = let time' = f 2.25  in x * time' * time' + 0.9375
    | otherwise      = let time' = f 2.625 in x * time' * time' + 0.984375
    where x = 7.5625
          y = 2.75
          f z = time - (z / y)


bounceIn :: Ord a => Ease a
bounceIn time = 1 - bounceOut (1 - time)


bounceInOut :: Ord a => Ease a
bounceInOut time = if time < 1 / 2
    then bounceIn  (time * 2)     * 0.5
    else bounceOut (time * 2 - 1) * 0.5 + 1 * 0.5


circIn :: Floating a => Ease a
circIn time =
    negate 1 * (sqrt (1 - time * time) - 1)


circOut :: Floating a => Ease a
circOut (negate -> time) =
    1 * sqrt (1 - time * time)


circInOut :: (Ord a, Floating a) => Ease a
circInOut ((* 2) -> time) =
    if time < 1
    then  negate 1 / 2 * (sqrt (1 - time  * time ) - 1)
    else let time' = time - 2
         in 1 / 2 * (sqrt (1 - time' * time') + 1)


cubicIn :: Ease a
cubicIn time =
    time * time * time


cubicOut :: Ease a
cubicOut (negate -> time) =
    time * time * time + 1


cubicInOut :: Ord a => Ease a
cubicInOut ((* 2) -> time) =
    if time < 1
    then    1 / 2 *  time  * time  * time
    else let time' = time - 2
         in 1 / 2 * (time' * time' * time' + 2)


newtype Amplitude a = Amplitude a
instance Num a => Default (Amplitude a) where def = Amplitude 0
data Period a = Period a | PeriodDefault
instance Default (Period a) where def = PeriodDefault


fromPeriod :: a -> Period a -> a
fromPeriod _ (Period a) = a
fromPeriod a _          = a


elasticOut :: forall a. (Eq a, Ord a, Floating a) => Amplitude a -> Period a -> Ease a
elasticOut _ _ 0 = 0
elasticOut _ _ 1 = 1
elasticOut (Amplitude amplitude) (fromPeriod 0.3 -> period) time =
    (amplitude' * 2 ** (negate 10 * time)) * sin ((time - overshoot) * 2 * pi / period) + 1
  where
    (amplitude', overshoot) =
        if amplitude < (1 :: a) then (1, period / 4)
        else (amplitude, period / (2 * pi) * asin (1 / amplitude))


elasticIn :: forall a. (Eq a, Ord a, Floating a) => Amplitude a -> Period a -> Ease a
elasticIn _ _ 0 = 0
elasticIn _ _ 1 = 1
elasticIn (Amplitude amplitude) (fromPeriod 0.3 -> period) time =
  negate (amplitude' * 2 ** (10 * time')) * sin ((time' - overshoot) * (2 * pi) / period)
  where
    time' = time - 1
    (amplitude', overshoot) =
        if amplitude < (1 :: a) then (1, period / 4)
        else (amplitude, period / (2 * pi) * asin (1 / amplitude))


elasticInOut :: forall a. (Eq a, Ord a, Floating a) => Amplitude a -> Period a -> Ease a
elasticInOut _ _ 0 = 0
elasticInOut _ _ 1 = 1
elasticInOut (Amplitude amplitude) (fromPeriod (0.3 * 1.5) -> period) time
  | time < 1   = negate 0.5 * (amplitude' * 2 **        (10 * time')) * sin ((time' - overshoot) * ((2 * pi) / period))
  | otherwise  =               amplitude' * 2 ** (negate 10 * time')  * sin ((time' - overshoot) *  (2 * pi) / period) + 1
  where
    time' = time - 1
    (amplitude', overshoot) =
        if amplitude < (1 :: a) then (1, period / 4)
        else (amplitude, period / (2 * pi) * asin (1 / amplitude))


expoIn :: (Eq a, Floating a) => Ease a
expoIn 0    = 0
expoIn time = 2 ** (10 * negate time)


expoOut :: (Eq a, Floating a) => Ease a
expoOut 1    = 1
expoOut time = negate (2 ** (negate 10 * time)) + 1


expoInOut :: (Eq a, Ord a, Floating a) => Ease a
expoInOut 0               = 0
expoInOut 1               = 1
expoInOut ((2 *) -> time) | time < 1 = 1 / 2 * 2 ** (10 * (time - 1))
expoInOut time            = 1 / 2 * negate (2 ** (negate 10 * (time - 1)) + 2)


linear :: Ease a
linear = (/ 1)


quadIn :: Ease a
quadIn time = time * time


quadOut :: Ease a
quadOut time = negate time * (time - 2)


quadInOut :: Ord a => Ease a
quadInOut ((* 2) -> time) =
    if time < 1
    then 1 / 2 * time * time
    else negate 1 / 2 * (time' * (time' - 2) - 1)
    where time' = time - 1


quartIn :: Ease a
quartIn time = time * time * time * time


quartOut :: Ease a
quartOut (negate -> time) = negate (time * time * time * time - 1)


quartInOut :: Ord a => Ease a
quartInOut ((* 2) -> time) =
    if time < 1
    then        1 / 2 *  time  * time  * time  * time
    else negate 1 / 2 * (time' * time' * time' * time' - 2)
    where time' = time - 2


quintIn :: Ease a
quintIn time = time * time * time * time * time


quintOut :: Ease a
quintOut (negate -> time) = time * time * time * time + 1


quintInOut :: Ord a => Ease a
quintInOut ((* 2) -> time) =
    if time < 1
    then 1 / 2 *  time  * time  * time  * time  * time
    else 1 / 2 * (time' * time' * time' * time' * time' + 2)
    where time' = time - 2


sineIn :: Floating a => Ease a
sineIn time = negate (cos (time * (pi / 2))) + 1


sineOut :: Floating a => Ease a
sineOut time = sin (time * (pi / 2))


sineInOut :: Floating a => Ease a
sineInOut time = negate 1 / 2 * (cos (pi * time) - 1)
