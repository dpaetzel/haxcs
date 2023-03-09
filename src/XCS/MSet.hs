{-|
Module      : XCS.MSet
Description : Match set
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

The non-empty set of classifiers matching a certain observation is called a
match set.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.MSet
  ( MSet
  , matchSet
  )
where


import CustomPrelude


import Control.Monad.Random.Class (MonadRandom)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE


import XCS.Classifier
import XCS.Conf
import XCS.Interface
import XCS.Population
import XCS.State


{-|
Match sets are non-empty lists since 'XCS.Conf.thetaMNA' always being greater
than zero means that if the population contains no matching classifiers covering
will create at least one.
-}
type MSet c a = NonEmpty (Classifier c a)


{-|
Statefully generates a match set given an observation: A collection of all
classifiers currently in the population that match that observation.
“Statefully” because any classifiers created through covering have to be
inserted into the population as well.

Corresponds to GENERATE MATCH SET in (Butz and Wilson, 2001).
-}
matchSet
  :: (Action a, Detector o c, MonadRandom m, MonadReader Conf m,
      Storage c a stor, MonadState (State stor c a) m)
  => o -> m (MSet c a)
matchSet o = (matching o <$> gets population) >>= cover o


{-|
Statefully generates new random classifiers that match the given observation
until the supplied set of classifiers satisfies the minimum number of actions
('XCS.Conf.thetaMNA').

In order to not need to perform the expensive matching operation on the whole
population repeatedly, the set of already existing matching classifiers is
provided to this function as well which recursively adds a single classifier to
it until it fulfils the 'XCS.Conf.thetaMNA' requirement.

Corresponds to GENERATE COVERING CLASSIFIER in (Butz and Wilson, 2000).
-}
cover
  :: (Action a, Detector o c, MonadRandom m, MonadReader Conf m,
      MonadState (State stor c a) m, Storage c a stor)
  => o -> [Classifier c a] -> m (MSet c a)
cover o cls = do
  conf <- ask
  if shouldCover (conf^.thetaMNA) cls
    then do
      t <- gets time
      cM <- coveringClassifier
            (conf^.pWild) (conf^.pI) (conf^.eI) (conf^.fI) t o cls
      let clsCovering = maybeToList cM
      insertM (conf^.nMicroMax) (conf^.thetaDel) (conf^.delta) clsCovering
      mset <- refresh' (cls ++ clsCovering)
      cover o mset
    else
      return $ NE.fromList cls


{-|
Given a value for 'XCS.Conf.thetaMNA', whether covering should be used in the
given of set of classifiers.
-}
shouldCover :: (Eq a) => Positive -> [Classifier c a] -> Bool
shouldCover thetaMNA cls = nA < weaken thetaMNA
  where
    nA = length . nub . fmap (^.rule.a) $ cls


{-|
Generates a covering classifier for the given match set given an observation of
the environment.
-}
coveringClassifier
  :: (Detector o c, Action a, MonadRandom m)
  => Real -> Real -> Real -> Real -> TimeStep -> o -> [Classifier c a]
  -> m (Maybe (Classifier c a))
coveringClassifier pWild pI eI fI t o mset = do
  condition <- coverCondition pWild o
  action <- missingAction mset
  return . fmap (flip (classifier pI eI fI condition) t) $ action


{-|
Returns a (random) action not yet proposed by the given match set.
-}
missingAction :: (Action a, MonadRandom r) => [Classifier c a] -> r (Maybe a)
missingAction = otherThan . fmap (^.rule.a)


{-|
Removes all the classifiers from the given set not currently in the population.
-}
refresh'
  :: (MonadState (State stor c a) m, Storage c a stor)
  => [Classifier c a] -> m [Classifier c a]
refresh' = filterM shouldKeep
  where
    shouldKeep cl = isJust . retrieve (cl^.rule) <$> gets population
