{-|
Module      : XCS.ASet
Description : Action sets (and prediction arrays)
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Action sets are collections of classifiers that propose the same action.

Prediction arrays consist of action sets that each propose a different action.
This differs from the original work (Butz and Wilson, 2001), where prediction
arrays are mere arrays of the system prediction values. However, the solution
used here seems to be more natural and also more general. While prediction
arrays are therefore not actual arrays, the original name was kept for
consistency.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}


module XCS.ASet
  ( SystemPrediction
  , systemPrediction
  , PArray
  , predictionArray
  , ASet(ASet)
  , proposition
  , prediction
  , proposing
  , asetSubsumption
  , refresh
  )
where


import CustomPrelude


import Control.Monad.Random.Class
import qualified Data.List.NonEmpty as NE
import Data.List ((\\))


import XCS.Classifier hiding (update)
import XCS.Interface
import XCS.MSet
import XCS.Population
import XCS.State


{-|
System predictions are real numbers.
-}
type SystemPrediction = Real


{-|
Returns the system prediction of the given non-empty set of classifiers. See
(Butz and Wilson, 2000).

Note that the resulting value only makes sense if all the classifiers propose
the same action.
-}
systemPrediction :: NonEmpty (Classifier c a) -> SystemPrediction
systemPrediction cls =
  (sum . fmap (\cl -> cl^.md.p * cl^.md.f) $ cls) / (sum . fmap (^.md.f) $ cls)


{-|
Prediction arrays are non-empty sets of action sets. They are non-empty because
match sets are non-empty (see 'XCS.MSet.MSet').

Note that contrary to the prediction array devised by (Butz and Wilson, 2001)
the classifiers themselves are a part of the prediction array as they are a part
of the underlying action sets. Actions that were not proposed by any classifier
of the population do not occur in prediction arrays.
-}
type PArray c a = NonEmpty (ASet c a NonEmpty)


{-|
Transforms the given match set into a prediction array.
-}
predictionArray :: (Eq a, Ord a) => MSet c a -> PArray c a
predictionArray =
  fmap aset .
    NE.groupBy1 ((==) `on` (^.rule.a)) .
    NE.sortBy (comparing (^.rule.a))
  where
    aset cls@(cl :| _) = ASet (cl^.rule.a) (systemPrediction cls) cls


{-|
Action sets are products of an action, a non-empty set of classifiers that all
propose that very action and the 'systemPrediction' of this action based on the
classifiers in the action set.

An action set is non-empty if it has just been created because match sets are
non-empty (see 'XCS.MSet.MSet') and action sets are only created for actions
occurring in a match set. However, when TD updating the estimations of the
classifiers in an action set, that set may be empty because the GA might have
been run in the meantime which could have led to the deletion of classifiers
from the population meaning that these classifiers should not exist in the
action set any more either. This duality is expressed by using different
collection types for the 'f' type variable in the ASet type constructor.
-}
data ASet c a f
  = ASet
  { proposition :: a
  -- ^ The action the classifiers in the action set propose.
  , prediction  :: SystemPrediction
  -- ^ The system prediction of the action set's classifiers for the
  -- proposition.
  , proposing   :: f (Classifier c a)
  -- ^ The collection of classifiers in the action set that propose the action.
  }


instance
  (Pretty c, Pretty a, forall x. Pretty x => Pretty (f x))
  => Pretty (ASet c a f) where
  pretty (ASet proposition prediction cls) =
    pretty proposition <+>
      text "⇒" <+>
      pretty prediction <>
      text "! ⇐" <+> pretty cls


{-|
Performs action set subsumption on the given action set using the supplied
parameters.
-}
asetSubsumption
  :: (Action a, Condition c, MonadRandom m, MonadState (State stor c a) m,
      Storage c a stor)
  => NonNegative
  -- ^ 'XCS.Conf.thetaSub'.
  -> Real
  -- ^ 'XCS.Conf.epsilon0'.
  -> ASet c a []
  -> m (ASet c a [])
asetSubsumption _ _ aset@(ASet _ _ []) = return aset
asetSubsumption thetaSub epsilon0 aset@(ASet _ _ cls@(cl : cls')) = do
  pop <- gets population
  -- the randomly selected most general classifier
  clGen <- oneMostGeneral thetaSub epsilon0 (cl :| cls')
  -- the subsumed classifiers
  let clsDel = filter (subsumes thetaSub epsilon0 clGen) cls
  -- delete the subsumed classifiers
  let pop' = foldl (flip delete) pop $ view rule <$> clsDel
  -- insert one instance of the general classifier for each subsumed classifier
  let pop'' = update (increaseN clsDel) (clGen^.rule) pop'
  modify (\s -> s { population = pop'' })
  -- remove the deleted classifier and update the most general classifier
  return aset { proposing = (cls \\ (clGen : clsDel)) ++ [clGen] }
  where
    increaseN clsDel = Just . over n (>+> (sum' $ view (md.n) <$> clsDel))


{-|
Selects one of the most general classifiers in the action set at random.
-}
oneMostGeneral
  :: (Action a, Condition c, MonadRandom m)
  => NonNegative
  -- ^ 'XCS.Conf.thetaSub'.
  -> Real
  -- ^ 'XCS.Conf.epsilon0'.
  -> NonEmpty (Classifier c a)
  -> m (Classifier c a)
oneMostGeneral thetaSub epsilon0 cls@(cl0 :| _) =
  foldl keepUnlessSubsumed cl0 <$> shuffleM cls
  where
    keepUnlessSubsumed clGen cl =
      if subsumes thetaSub epsilon0 cl clGen
      then cl
      else clGen


{-|
Updates the action set with the classifier versions currently in the population
(also deleting any classifiers not in the population any more).
-}
refresh
  :: (Foldable f, Functor f, MonadState (State stor c a) m, Storage c a stor)
  => ASet c a f -> m (ASet c a [])
refresh aset = do
  pop <- gets population
  return $ aset { proposing = retrieveAll pop }
  where
    retrieveAll pop =
      concatMaybes $ (`retrieve` pop) <$> ((^.rule) <$> proposing aset)
    concatMaybes = foldl (\x m -> case m of Just v -> v : x; _ -> x) []
