{-|
Module      : XCS.Interface.Detector
Description : Relationship between observation and rule types
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Relationship between observation and rule types.
-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Interface.Mutator where


import CustomPrelude


import           Control.Monad.Random.Class


import           XCS.Interface.Action
import           XCS.Interface.Condition
import           XCS.Classifier.Rule


{-|
The 'Mutator' class defines the necessary relationship between observation and
rule types: XCS' genetic algorithm needs to be able to mutate rules without
violating the most recent observation.
-}
class (Condition c, Action a) => Mutator o c a where
  {-|
  Incorporating a probability (see 'XCS.Conf.mu') for a gene's mutation and an
  observation that the result still needs to conform to (mutation in XCS never
  makes a classifier leave the current niche), returns a random monad
  computation yielding a mutated version of the given rule.

  An example implementation is the one for bit strings in
  'XCS.Interface.BitString.Mutator'.
  -}
  mutate :: (MonadRandom m) => Probability -> o -> Rule c a -> m (Rule c a)
