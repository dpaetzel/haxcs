{-|
Module      : XCS.GeneticAlgorithm
Description : Genetic algorithm for optimizing classifier conditions
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

XCS's genetic algorithm (GA) gradually optimizes the population regarding the
classifiers' conditions.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}


{-# OPTIONS_GHC -Wno-type-defaults #-}


module XCS.GeneticAlgorithm
  ( runGA
  )
where


import CustomPrelude


import Control.Monad.Random


import XCS.Classifier hiding (update)
import XCS.Conf as Conf
import XCS.Population (Storage, update)
import XCS.Interface
import XCS.State
import XCS.Util (roulette)


{-|
Statefully executes XCS's GA once—if it is time for it—on the given set of
classifiers, 'XCS.State.insertM'ing (thus, with deletion) newly generated
classifiers into the population.
-}
runGA
  :: (Condition c, Eq a, MonadRandom m, MonadReader Conf m,
      MonadState (State stor c a) m, Mutator o c a, Storage c a stor)
  => [Classifier c a] -> o -> m ()
runGA cls o = do
  conf <- ask
  t <- gets time
  when (shouldRun (conf^.thetaGA) t cls) $ do
    updateGATime t cls
    clsM <- nicheGA
      (conf^.chi)
      (conf^.mu)
      (conf^.doGASubsumption)
      (conf^.thetaSub)
      (conf^.epsilon0)
      cls
      o
    case clsM of
      Just (cl1, cl2) ->
        insertM (conf^.nMicroMax) (conf^.thetaDel) (conf^.delta) [cl1, cl2]
      Nothing -> return ()


{-|
Decides based on the current time, the GA threshold and the given set of
classifiers whether the GA should be run on that set.
-}
shouldRun :: NonNegative -> TimeStep -> [Classifier c a] -> Bool
shouldRun _       _ []  = False
shouldRun thetaGA t cls =
  let
    denom = fromIntegral . sum . fmap (\cl -> cl^.md.tGA * num' cl) $ cls
    numer = fromIntegral . sum . fmap num' $ cls
  in
    (fromIntegral t - denom / numer > toNum thetaGA)


{-|
Statefully updates the 'XCS.Classifier.Metadata.tGA' metadata field of the given
set of classifiers to the given timestamp.
-}
updateGATime
  :: (Foldable f, Functor f, MonadState (State stor c a) m, Storage c a stor)
  => TimeStep -> f (Classifier c a) -> m ()
updateGATime t cls = do
  pop <- gets population
  e <- gets ep
  let ls = fmap (^.rule) cls
  let pUpdated = foldr (update (\md -> Just $ md { _tGA = t })) pop ls
  put (State pUpdated t e)
  -- TODO add state lenses


{-|
Selects parents from the given set of classifiers using fitness-based roulette
wheel selection and combines them using crossover (with the given probability).
The created offspring is mutated (with the given probability) and returned.

If GA subsumption is enabled then possibly one or both of the parents are
returned instead if they subsume their offspring.
-}
nicheGA
  :: (MonadRandom m, Mutator o c a)
  => Real
  -- ^ 'XCS.Conf.chi'.
  -> Real
  -- ^ 'XCS.Conf.mu'.
  -> Bool
  -- ^ 'XCS.Conf.doGASubsumption'.
  -> NonNegative
  -- ^ 'XCS.Conf.thetaSub'.
  -> Real
  -- ^ 'XCS.Conf.epsilon0'.
  -> [Classifier c a]
  -- ^ Set of classifiers to use the GA on.
  -> o
  -- ^ Observation that mutation should respect.
  -> m (Maybe (Classifier c a, Classifier c a))
nicheGA _   _  _               _        _        []           _ = return Nothing
nicheGA chi mu doGASubsumption thetaSub epsilon0 (cl' : cls') o = do
  cl1@(Classifier l1 md1) <- roulette $ votes cls
  cl2@(Classifier l2 md2) <- roulette $ votes cls
  shouldCrossover <- (< chi) <$> getRandomR (0, 1 :: Real)
  (cX1, cX2) <-
    if shouldCrossover
    then crossover 2 (l1^.c) (l2^.c)
    else return (l1^.c, l2^.c)
  lXM1 <- mutate mu o $ Rule cX1 (l1^.a)
  lXM2 <- mutate mu o $ Rule cX2 (l2^.a)
  let (mdXM1, mdXM2) = reset md1 md2
  let clXM1 = Classifier lXM1 mdXM1
  let clXM2 = Classifier lXM2 mdXM2
  return . Just $
    if doGASubsumption
      then subsumers thetaSub epsilon0 cl1 cl2 clXM1 clXM2
      else (clXM1, clXM2)
  where
    cls = cl' :| cls'
    subsumers thetaSub epsilon0 cl1 cl2 clXM1 clXM2 = (select clXM1, select clXM2)
      where
        select clXM
          | subsumes thetaSub epsilon0 cl1 clXM = cl1
          | subsumes thetaSub epsilon0 cl2 clXM = cl2
          | otherwise = clXM


{-|
The votes of the given non-empty set of classifiers in the GA's roulette wheel
selection.
-}
votes :: NonEmpty (Classifier c a) -> NonEmpty (Classifier c a, Real)
votes = fmap (\cl -> (cl, cl^.md.f))


{-|
Creates offspring metadata from the two given parent metadata objects.
-}
reset :: Metadata -> Metadata -> (Metadata, Metadata)
reset md1 md2 = (reset' md1 md2, reset' md2 md1)
  where
    reset' md1 md2 = md1
      { _p = (md1^.p + md2^.p) / 2
      , _e = (md1^.e + md2^.e) / 2
      , _f = 0.1 * (md1^.f + md2^.f) / 2
      , _x = nonNegative 0
      , _n = positive 1
      }
