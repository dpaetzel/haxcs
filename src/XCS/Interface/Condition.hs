{-|
Module      : XCS.Interface.Condition
Description : Condition class
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Class of condition types.
-}


{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Interface.Condition where


import CustomPrelude


import Control.Monad.Random.Class


{-|
Conditions are required to be comparable as well as to implement a number of
functions.
-}
class (Eq c, Ord c, Pretty c) => Condition c where
  {-# MINIMAL generality, generalizes, crossover1 #-}
  {-|
  Returns the condition's generality.
  -}
  generality :: c -> Real

  {-|
  Whether the first condition generalizes the second.
  -}
  infixl 7 `generalizes`
  generalizes :: c -> c -> Bool

  {-|
  Performs a single-point crossover on two conditions, returning a random monad
  computation yielding two new conditions.

  Implementations should probably choose the crossover position at random.
  -}
  crossover1 :: (MonadRandom m) => c -> c -> m (c, c)

  -- TODO use NonNegative instead of Integer
  {-|
  Performs an n-point crossover on two conditions.

  Given the function for single-point crossover, 'crossover1', this function can
  be derived through recursion and a monad combinator (which is also the default
  implementation).
  -}
  crossover :: (MonadRandom m) => Integer -> c -> c -> m (c, c)
  crossover n c1 c2
    | n <= 0    = return (c1, c2)
    | otherwise =
        crossover1 c1 c2 >>= uncurry (crossover (n - 1))
