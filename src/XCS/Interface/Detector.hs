{-|
Module      : XCS.Interface.Detector
Description : Relationship between observation and condition types
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Relationship between observation and condition types.
-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Interface.Detector where


import CustomPrelude


import Control.Monad.Random.Class


import XCS.Interface.Condition


{-|
The 'Detector' class defines the necessary relationship between condition and
observation types: An XCS matches incoming observations of type @o@ against
conditions of type @c@.
-}
class (Condition c) => Detector o c where
  infixl 7 `matchedBy`
  {-|
  Whether the given observation is matched by the given condition.
  -}
  matchedBy :: o -> c -> Bool
  {-|
  Incorporating the supplied probability for a higher generality (see
  'XCS.Conf.pWild'), returns a random monad computation yielding a new
  condition that matches the given observation.

  An example for the usage of the probability parameter can be seen in the
  implementation for bit strings in 'XCS.Interface.BitString.Detector'. There,
  the probability is used as the probability of using a wildcard.
  -}
  coverCondition :: (MonadRandom m) => Probability -> o -> m c
