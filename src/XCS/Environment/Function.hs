{-|
Module      : XCS.Environment.Function
Description : Function environment
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

A reinforcement learning environment based on a single function that maps each
state (observation) to the best possible action in that state. This best
possible action yields a reward of 1 whereas all other actions yield a 0 reward.
-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Environment.Function where


import CustomPrelude hiding (run)


import Control.Monad.Random.Lazy


import XCS.Conf (Conf)
import XCS.Environment
import XCS.Interface (Detector, Mutator)
import XCS.Log (Log)
import XCS.Population (Storage)
import XCS.Run (run, runXCS)
import XCS.State (State)


{-|
A function-based reinforcement learning environment is monad transformer stack
consisting of

 - a 'Control.Monad.ReaderT' monad transformer to read the function @f :: o ->
   t@ it is based on from,

 - a 'Control.Monad.StateT' monad transformer to hold the tick counter and the
   observation of type @o@ the environment provided last (if an observation @o@
   was provided last, until the next observation took place, the only action
   yielding a reward is @f o@—this means that @t@ is the type of actions this
   environment supports) and

 - a 'Control.Monad.Random.Lazy.Rand' monad for a random generator of type @g@
   to be able to generate random observations.
-}
newtype FunctionEnvironment g o t x = FunctionEnvironment
  { runFunctionEnvironment''
    :: ReaderT (o -> t) (StateT (Integer, Maybe o) (Rand g)) x
    -- ^ You probably want to use 'runFunctionEnvironment' instead.
  }
  deriving
    (Applicative, Functor, Monad, MonadRandom, MonadReader (o -> t),
     MonadState (Integer, Maybe o))


instance
  (Eq t, Random o, RandomGen g)
  => (MonadEnvironment o t) (FunctionEnvironment g o t) where
  observe = do
    (t, _) <- get
    o <- getRandom
    put (t + 1, Just o)
    return o
  -- NOTE act will always return a reward of 0 if used without observing first.
  act a = do
    (_, oPrevious) <- get
    f <- ask
    if (f <$> oPrevious) == Just a
      then return 1
      else return 0
  eop = return True


{-|
A simpler to use alternative to 'runFunctionEnvironment'''.

You may want to use 'runFunctionEnvironment' though.
-}
runFunctionEnvironment'
  :: FunctionEnvironment g o t x
  -> (o -> t)
  -- ^ The function to base the reinforcement learning environment on.
  -> g
  -- ^ The intial random generator used for sampling by the function
  -- environment.
  -> ((x, (Integer, Maybe o)), g)
runFunctionEnvironment' e f =
  runRand (runStateT (runReaderT (runFunctionEnvironment'' e) f) (0, Nothing))


{-|
The function-based environment monad stack applied to the XCS monad stack.

This is just another alternative to 'runFunctionEnvironment''' that is even
simpler to use than 'runFunctionEnvironment'': the result of
@runFunctionEnvironment' (runXCS …)@ has a type of @((((((), State stor c a),
StdGen), Log), (Integer, Maybe o)), StdGen)@—'runFunctionEnvironment' extracts
the most important parts from that.
-}
runFunctionEnvironment
  :: (Detector o c, Mutator o c a, Random o, Storage c a stor)
  => Conf
  -- ^ 'XCS.XCS' configuration to use.
  -> Int
  -- ^ Seed for the random generator.
  -> State stor c a
  -- ^ Initial state of the 'XCS.XCS'.
  -> (o -> a)
  -- ^ The function to base the reinforcement learning environment on.
  -> (State stor c a, Log)
  -- ^ The final state of the 'XCS.XCS''s 'Control.Monad.StateT' monad
  -- transformer and the log written by its 'Control.Monad.WriterT' monad
  -- transformer.
runFunctionEnvironment conf s init f =
  let
    gen = mkStdGen s
    (((((_, st), _), log), _), _) =
      runFunctionEnvironment' (runXCS run conf init gen) f gen
  in
    (st, log)
