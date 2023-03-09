{-|
Module      : XCS.Population
Description : Classifier population
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

An XCS's 'XCS.Classifier.Classifier's are held in a data structure called its
population. A population uses a 'XCS.Classifier.Storage' to store the
macro-classifiers.
-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}


module XCS.Population
  ( Storage
  -- * Population type and creation
  , Population
  , empty
  -- * Population size
  , nMicro
  , nMacro
  -- * Lookup
  , retrieve
  , matching
  -- * Changing populations
  , insert
  , delete
  , update
  , reinforce
  -- * Logging
  , avgError
  , avgFitness
  , avgGenerality
  , generalityHistogram
  , csv
  )
where


import CustomPrelude hiding (empty)


import Control.Monad.Random (MonadRandom)


-- Logging
import Data.Csv as CSV
import qualified Data.Map.Strict as M
import qualified XCS.Interface as I (generality)


import XCS.Classifier hiding (update)
import qualified XCS.Classifier as C
import XCS.Interface (Condition, Detector, generality, matchedBy)
import XCS.Storage (Storage)
import qualified XCS.Storage as Storage
import XCS.Util (roulette)


{-|
For each storage type, a corresponding population type is defined.
-}
newtype (Storage c a stor) => Population stor c a = Population (stor c a)


deriving instance
  (Show (stor c a), Storage c a stor) => Show (Population stor c a)


instance
  (Pretty (stor c a), Storage c a stor)
  => Pretty (Population stor c a) where
  pretty (Population stor) = pretty stor


{-|
An empty population.
-}
empty :: (Ord c, Ord a, Storage c a stor) => Population stor c a
empty = Population Storage.empty


{-|
The number of micro-classifiers in the population.

This is not to be confused with the size of the population's storage (the number
of macro-classifiers, see 'XCS.Population.nMacro' and 'XCS.Storage.size').
-}
nMicro :: (Ord c, Ord a, Storage c a stor) => Population stor c a -> NonNegative
nMicro (Population stor) =
  Storage.foldr (\cl k -> weaken (cl^.md.n) >=+>= k) (nonNegative 0) stor


{-|
The number of macro-classifiers in the population.

This equals the size of the population's storage (see 'XCS.Storage.size').
-}
nMacro :: (Ord c, Ord a, Storage c a stor) => Population stor c a -> NonNegative
nMacro (Population stor) = Storage.size stor


{-|
The classifiers in the population whose conditions match the given
observation.
-}
matching
  :: (Detector o c, Ord c, Ord a, Storage c a stor)
  => o -> Population stor c a -> [Classifier c a]
matching o (Population stor) =
  Storage.filter (matchedBy o . view (rule.c)) stor


{-|
If a classifier @cl@ with @cl^.rule == l@ exists in population @p@, @retrieve l
p == Just cl@ (otherwise @retrieve l p == Nothing@).
-}
-- TODO this is too low-level (only needed by refresh in mset and XCS module)
retrieve
  :: (Storage c a stor)
  => Rule c a -> Population stor c a -> Maybe (Classifier c a)
retrieve l (Population stor) = Storage.retrieve l stor


{-|
The population with the given list of classifiers inserted. Except for their
numerosity, the metadata of already existing classifiers with the same rules is
discarded.

Like INSERT IN POPULATION in (Butz and Wilson, 2000) but with one difference:
instead of only a single classifier, a list of classifiers is inserted.
-}
insert
  :: (Ord c, Ord a, Storage c a stor, MonadRandom m)
  => Positive
  -- ^ The maximum number of classifiers allowed in this population (see
  -- 'XCS.Config.n').
  -> NonNegative
  -- ^ The deletion threshold (see 'XCS.Config.thetaDel').
  -> Real
  -- ^ The fraction of mean fitness threshold (see 'XCS.Config.delta').
  -> [Classifier c a]
  -- ^ The classifiers to insert.
  -> Population stor c a
  -- ^ The population to insert the classifiers in.
  -> m (Population stor c a)
insert nMax thetaDel delta cls (Population stor) =
  prune nMax thetaDel delta . Population . foldr Storage.insert stor $ cls


{-|
If the population's size @m@ exceeds the configured maximum size @n conf@,
deletes @m - n@ classifiers from the current population. The classifiers are
selected for deletion using a fitness-based roulette wheel.
-}
prune
  :: (Ord c, Ord a, Storage c a stor, MonadRandom m)
  => Positive
  -- ^ The maximum number of classifiers allowed in this population (see
  -- 'XCS.Config.n').
  -> NonNegative
  -- ^ The deletion threshold (see 'XCS.Config.thetaDel').
  -> Real
  -- ^ The fraction of mean fitness threshold (see 'XCS.Config.delta').
  -> Population stor c a
  -- ^ The population to prune.
  -> m (Population stor c a)
prune n thetaDel delta pop =
  if shouldPrune then
    pruneOnce thetaDel delta pop >>= prune n thetaDel delta
  else
    return pop
  where
    shouldPrune = (> weaken n) . nMicro $ pop


{-|
Select one classifier using fitness-based roulette wheel selection and delete
it.
-}
pruneOnce
  :: (Ord c, Ord a, Storage c a stor, MonadRandom m)
  => NonNegative
  -- ^ The deletion threshold (see 'XCS.Config.thetaDel').
  -> Real
  -- ^ The fraction of mean fitness threshold (see 'XCS.Config.delta').
  -> Population stor c a
  -- ^ The population to prune once.
  -> m (Population stor c a)
pruneOnce thetaDel delta pop =
  case votes thetaDel delta pop of
    (v : vs) -> flip delete1 pop . view rule <$> roulette (v :| vs)
    _ -> return pop


{-|
The votes of a population's classifiers for pruning selection.
-}
votes
  :: (Ord c, Ord a, Storage c a stor)
  => NonNegative
  -- ^ The deletion threshold (see 'XCS.Config.thetaDel').
  -> Real
  -- ^ The fraction of mean fitness threshold (see 'XCS.Config.delta').
  -> Population stor c a
  -- ^ The population to calculate votes for.
  -> [(Classifier c a, Real)]
votes thetaDel delta p@(Population stor) =
  Storage.foldr prependVote [] stor
  where
    prependVote cl vs = (cl, vote thetaDel delta (avgFitness p) cl) : vs


{-|
A classifier's vote during the pruning selection.

From (Butz and Wilson, 2000):

@
DELETION VOTE(cl, [P]):
vote <- as cl * num cl
avgFitnessInPopulation <- (sum . fmap f $ p) / sum . fmap num $ p)
if (exp cl > thetaDel conf) and f cl / num cl < delta * avgFitnessInPopulation)
  vote <- vote * averageFitnessInPopulation / (f cl / num cl)
return vote
@
-}
vote
  :: NonNegative
  -- ^ The deletion threshold (see 'XCS.Config.thetaDel').
  -> Real
  -- ^ The fraction of mean fitness threshold (see 'XCS.Config.delta').
  -> Fitness
  -- ^ The average fitness in the population the supplied classifier belongs to.
  -> Classifier c a
  -- ^ The classifier to calculate a vote for.
  -> Real
vote thetaDel delta avgFitness cl =
  cl^.md.aAvg * num' cl
  * if cl^.md.x > thetaDel && fMicro < delta * avgFitness then
      avgFitness / fMicro
    else
      1
  where
    fMicro = cl^.md.f / num' cl


{-|
Deletes the macro-classifier with the given rule (i. e. regarding numerosity,
deletes *all* instances of it).

Required for aset subsumption.
-}
delete
  :: (Ord c, Ord a, Storage c a stor)
  => Rule c a -> Population stor c a -> Population stor c a
delete l (Population stor) = Population $ Storage.update (const Nothing) l stor


{-|
If it exists, deletes one instance of the classifier (that is, one
micro-classifier) with the given rule from the population (i. e. deletes the
macro-classifier altogether if its numerosity is 1 or decreases its numerosity
by one).

Required for population pruning.
-}
delete1
  :: (Ord c, Ord a, Storage c a stor)
  => Rule c a -> Population stor c a -> Population stor c a
delete1 l (Population stor) = Population $ Storage.update numMinusMinus l stor


{-|
@update f l p@ updates the metadata of the classifier with rule @l@ in @p@ using
@f@. If @f@ returns @Nothing@, the classifier is deleted.
-}
update
  :: (Ord c, Ord a, Storage c a stor)
  => (Metadata -> Maybe Metadata) -> Rule c a -> Population stor c a
  -> Population stor c a
update f l (Population stor) = Population $ Storage.update f l stor


{-|
Updates the given (macro-)classifiers in the population according to the given
reward.
-}
reinforce
  :: (Storage c a stor, Ord c, Ord a)
  => Real
  -- ^ beta
  -> Real
  -- ^ epsilon0
  -> Real
  -- ^ alpha
  -> Real
  -- ^ nu
  -> [Classifier c a]
  -> Reward
  -> Population stor c a
  -> Population stor c a
reinforce beta epsilon0 alpha nu cls r pop = foldr update1 pop msetUpdated
  where
    update1 cl = update (\_ -> Just $ cl^.md) (cl^.rule)
    msetUpdated = C.update beta epsilon0 alpha nu r cls


{-|
The average fitness ('XCS.Classifier.Metadata._f') of the classifiers in the
given population.
-}
avgFitness :: (Ord c, Ord a, Storage c a stor) => Population stor c a -> Fitness
avgFitness pop@(Population stor) = Storage.foldr f' 0 stor / toNum (nMicro pop)
  where
    f' cl s = cl^.md.f * num' cl + s


{-|
The average error ('XCS.Classifier.Metadata._e') of the classifiers in the given
population.
-}
avgError :: (Ord c, Ord a, Storage c a stor) => Population stor c a -> Error
avgError pop@(Population stor) = Storage.foldr e' 0 stor / toNum (nMicro pop)
  where
    e' cl s = cl^.md.e * num' cl + s


{-|
The average generality of the classifiers in the given population.
-}
avgGenerality
  :: (Condition c, Ord a, Storage c a stor)
  => Population stor c a -> Real
avgGenerality pop@(Population stor) =
  (/ toNum (nMacro pop)) . Storage.foldr generality' 0 $ stor
  where
    generality' cl x = x + generality (cl^.rule.c)


{-|
Outputs the population as a CSV byte string.
-}
csv
  :: (Storage c a stor, ToField c, ToField a)
  => Population stor c a -> LByteString
csv (Population stor) = CSV.encode . Storage.foldr (:) [] $ stor


{-|
A histogram of generalities.
-}
generalityHistogram
  :: (Condition c, Storage c a stor) => Population stor c a -> [(Real, Real)]
generalityHistogram (Population stor) =
  M.toList . Storage.foldr f M.empty $ stor
  where
    f cl = M.insertWith (+) (I.generality $ cl^.rule.c) 1
