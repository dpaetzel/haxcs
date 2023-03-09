{-|
Module      : XCS.State
Description : XCS's “internal state”
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

The state managed by XCS.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}


-- for the implementations of trace{,Show}T
{-# OPTIONS_GHC -Wno-deprecations #-}


module XCS.State where


import CustomPrelude


import Control.Monad.Random (MonadRandom)


import XCS.Classifier
import XCS.Population
import XCS.Storage (Storage)


{-|
Type alias to increase readability. Time in XCS is tick-based.
-}
type TimeStep = Integer


{-|
XCS was originally defined in such a way as to repeatedly update two values: The
'XCS.Population' and a step counter. Both of these are therefore part of XCS's
state.
-}
data State stor c a
  = State
    { population :: Population stor c a
    , time       :: TimeStep
    , ep         :: NonNegative
    }
  deriving (Show)


-- TODO
-- instance (Storage stor c a, Arbitrary (stor c a), Arbitrary c, Arbitrary a) =>
--   Arbitrary (State stor c a) where
--   arbitrary = do
--     population <- arbitrary
--     State <$>
--       return population <*>
--       arbitrary `suchThat` (>= 0)


{-|
Helper function that simply inserts the given set of classifiers into the
current state's population.
-}
insertM
  :: (MonadRandom m, MonadState (State stor c a) m, Storage c a stor)
  => Positive -> NonNegative -> Real -> [Classifier c a] -> m ()
insertM nMicroMax thetaDel delta cls = do
  pop <- gets population >>= insert nMicroMax thetaDel delta cls
  modify (\s -> s { population = pop })


{-|
'trace' with a time stamp.
-}
{-# WARNING traceM "'XCS.State.traceM' remains in code" #-}
traceM :: (MonadState (State stor c a) s) => [Char] -> s ()
traceM desc = do
  t <- gets time
  trace (show t ++ ": " ++ desc) $ return ()


{-|
'traceShow' with a time stamp.
-}
{-# WARNING traceShowM "'XCS.State.traceShowM' remains in code" #-}
traceShowM :: (MonadState (State stor c a) s, Show x) => [Char] -> x -> s ()
traceShowM desc x = do
  t <- gets time
  trace (show t ++ " " ++ desc ++ ": " ++ show x) $ return ()
