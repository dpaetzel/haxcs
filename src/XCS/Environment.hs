{-|
Module      : XCS.Environment
Description : Environment monad class
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Class of reinforcement learning environment monads.
-}


{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Environment where


import CustomPrelude


{-|
Reinforcement learning environments are monads that support the three functions
'observe', 'act' and 'eop'. At that, @o@ and @a@ are the types for observations
and actions respectively.
-}
class (Monad m) => MonadEnvironment o a m | m -> o, m -> a where
  observe :: m o
  -- ^ Generate an observation for a reinforcement learning agent.
  act :: a -> m Reward
  -- ^ Execute the given action and receive a reward.
  eop :: m Bool
  -- ^ Signal whether the environment is in a terminal state, meaning that the
  -- end of problem (or of the current episode) is reached. The environment of a
  -- continuous task should always return 'False' here.
