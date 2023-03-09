{-|
Module      : XCS
Description : XCS runs, episodes and steps
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

The type for XCS computations is a stack of several monads transformers.

It is used to define XCS runs as well as reinforcement learning episodes and
steps.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}


module XCS.Run
  ( XCS
  , runXCS'
  , runXCS
  , run
  , episode
  , step
  )
where


import CustomPrelude hiding (run)


import Control.Monad.Random (RandomGen, RandT, runRandT)
import Control.Monad.Random.Class
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT)
import Data.List (maximum)


import XCS.ActionSelection
import XCS.ASet
import XCS.Conf
import XCS.Environment as E
import XCS.GeneticAlgorithm
import XCS.Interface
import XCS.Log (Log, logEpisode, logStep)
import XCS.Mode
import XCS.MSet
import XCS.Population (Storage, reinforce)
import XCS.State


{-|
The type of an XCS computation. Its definition can be interpreted as: Given
types for a system monad (@io@), a random generator (@g@), a population storage
(@stor@), observations (@o@), conditions (@c@) and actions (@a@), an XCS
computation is a computation dependent on the external system augmented with
randomness-dependence, the state monad managing a @State stor c a@ and the
configuration supplied by the reader monad.

[@io@]: Type for the system monad (for observing and acting)

[@g@]: Random generator type

[@stor@]: Population storage type

[@o@]: Observation type

[@c@]: Condition type

[@a@]: Action type
-}
newtype XCS io g stor o c a x = XCS
  { runXCS'
    :: ReaderT Conf (StateT (State stor c a) (RandT g (WriterT Log io))) x
    -- ^ Runs an XCS computation and extracts the final value from it.
    --
    -- Consider using 'runXCS'.
    --
    -- Not to be confused with 'run' which defines an XCS computation describing
    -- one run of XCS.
  }
  deriving
    (Applicative, Functor, Monad, MonadRandom, MonadReader Conf,
     MonadState (State stor c a), MonadWriter Log)


instance
  (MonadEnvironment o a io)
  => MonadEnvironment o a (XCS io g stor o c a) where
  observe = XCS . lift . lift . lift . lift $ E.observe
  act = XCS . lift . lift . lift . lift . E.act
  eop = XCS . lift . lift . lift . lift $ E.eop


{-|
A simpler to use alternative to 'runXCS''.

Not to be confused with 'run' which defines an XCS computation describing one
run of XCS.
-}
runXCS
  :: (MonadEnvironment o a io)
  => XCS io g stor o c a x
  -> Conf
  -> State stor c a
  -> g
  -> io (((x, State stor c a), g), Log)
runXCS xcs conf s g =
  runWriterT (runRandT (runStateT (runReaderT (runXCS' xcs) conf) s) g)


{-|
An XCS run consists of repeated 'episode's until the termination criterion is
met (see 'XCS.Conf.terminateWhen').
-}
run
  :: (MonadEnvironment o a io, Detector o c, Mutator o c a, RandomGen g,
      Storage c a stor)
  => XCS io g stor o c a ()
run = untilM terminateNow episode
  where
    terminateNow = gets ep >>= (\t -> view terminateWhen <$> ask <*> return t)


{-|
An episode consists of repeated 'step's until the environment sends the end of
episode signal (see 'XCS.Environment.eop').
-}
episode
  :: (MonadEnvironment o a io, Detector o c, Mutator o c a, RandomGen g,
      Storage c a stor)
  => XCS io g stor o c a ()
episode = do
  episode <- catMaybes <$> iterateUntilM' eop step Nothing
  -- note that the following block is only required for logging
  unless (null episode) $ do
    let ret = sum . fmap (\(_, _, r) -> r) $ episode
    ep' <- gets ep
    logEpisode ep' ret
  increaseEpM


{-|
Increase the episode counter by one.
-}
increaseEpM :: (MonadState (State stor c a) m) => m ()
increaseEpM = modify (\s -> s { ep = ep s >=+>= nonNegative 1 })


{-|
A step consists of one interaction with the environment. If after that no
terminal state is reached, the classifiers of the previous step's action set are
updated (if the argument to step is not 'Nothing' which is the case if that step
was an exploratory one).
-}
step
  :: (MonadEnvironment o a io, Detector o c, Mutator o c a, RandomGen g,
      Storage c a stor)
  => Maybe (o, ASet c a [], Reward)
  -- ^ Observation, action set and reward from the previous step (if it was an
  -- exploratory one).
  -> XCS io g stor o c a (Maybe (o, ASet c a [], Reward))
step d1 = do
  (mod, o, aset, r, pSystem) <- interact
  update1 d1 pSystem
  aset' <- refresh aset
  if mod == Exploring
    then do
      whenM eop $ updateEOP (o, aset', r)
      aset'' <- refresh aset'
      State pop t ep <- get
      logStep pop t ep pSystem mod r
      increaseTimeM $ Just (o, aset'', r)
    else do
      State pop t ep <- get
      logStep pop t ep pSystem mod r
      increaseTimeM Nothing


{-|
Interacts with the environment once.
-}
interact
  :: (MonadEnvironment o a io, Detector o c, Mutator o c a, RandomGen g,
      Storage c a stor)
  => XCS io g stor o c a (Mode, o, ASet c a NonEmpty, Reward, SystemPrediction)
interact = do
  o <- observe
  mset <- matchSet o
  let parray = predictionArray mset
  p <- view pExplr <$> ask
  (mod, aset) <- selectEpsilonGreedy p parray
  r <- act . proposition $ aset
  let pSystem = maximum $ prediction <$> parray
  return (mod, o, aset, r, pSystem)


{-|
Uses an ε-greedy policy based on the given prediction array: Selects a random
(exploring) action with the given probability (see 'XCS.Conf.pExplr') and a
greedy (exploiting) action otherwise (ties broken randomly).
-}
selectEpsilonGreedy :: (Action a, MonadRandom m) =>
  Probability -> PArray c a -> m (Mode, ASet c a NonEmpty)
selectEpsilonGreedy pExplr parray = do
  mod <- mode pExplr
  if mod == Exploring
    then (Exploring,) <$> uniform parray
    else (Exploiting,) <$> selectGreedy parray


{-|
Updates the action set's classifiers, performs action set subsumption (if
enabled by the configuration) and runs the GA—all of that on the data gathered
in the previous step (that is, [A]_-1 etc.).
-}
update1
  :: (MonadEnvironment o a io, Detector o c, Mutator o c a, RandomGen g,
      Storage c a stor)
  => Maybe (o, ASet c a [], Reward)
  -- ^ observation, action set and reward from the last step (if there was one
  -- and it was exploratory).
  -> SystemPrediction
  -- ^ System prediction from the current step.
  -> XCS io g stor o c a ()
update1 Nothing _ = return ()
update1 (Just (o1, aset1, r1)) pSystem = do
  conf <- ask
  let r1' = r1 + conf^.gamma * pSystem
  -- TODO use some nice lens here
  modify (\s -> s { population =
    reinforce (conf^.beta) (conf^.epsilon0) (conf^.alpha) (conf^.nu)
        (proposing aset1) r1' $ population s })
  aset1' <-
    if conf^.doASetSubsumption
      then asetSubsumption (conf^.thetaSub) (conf^.epsilon0) aset1
      else return aset1
  runGA (proposing aset1') o1


{-|
Updates the action set's classifiers, performs action set subsumption (if
enabled by the configuration) and runs the GA—all of that on the data gathered
in the current step (that is, [A] etc.).

This meant to only be run if the current state is terminal, that is, the episode
ended.
-}
updateEOP
  :: (MonadEnvironment o a io, Detector o c, Mutator o c a, RandomGen g,
      Storage c a stor)
  => (o, ASet c a [], Reward)
  -- ^ Observation, action set, reward and system prediction from the current
  -- step.
  -> XCS io g stor o c a ()
updateEOP (o, aset, r) = do
  conf <- ask
  -- TODO use some nice lens here
  modify (\s -> s { population =
    reinforce (conf^.beta) (conf^.epsilon0) (conf^.alpha) (conf^.nu)
      (proposing aset) r $ population s })
  let aset' = aset { proposing = proposing aset }
  aset'' <-
    if conf^.doASetSubsumption
      then asetSubsumption (conf^.thetaSub) (conf^.epsilon0) aset'
      else return aset'
  runGA (proposing aset'') o


{-|
Increase the tick counter by one. The argument is just passed through without
modification or effect (for better composability).
-}
increaseTimeM :: (MonadState (State stor c a) m) => x -> m x
increaseTimeM x = modify (\s -> s { time = time s + 1 }) >> return x

