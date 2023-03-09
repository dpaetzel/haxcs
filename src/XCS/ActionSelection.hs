{-|
Module      : XCS.ActionSelection
Description : Action selection methods
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

A collection of functions for action selection.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.ActionSelection where


import CustomPrelude


import Control.Monad.Random.Class
import qualified Data.List.NonEmpty as NE


import XCS.Interface (Action)
import XCS.ASet


{-|
Selects one of the best actions based on the given prediction array (ties broken
randomly).
-}
selectGreedy
  :: (Action a, MonadRandom m)
  => PArray c a -> m (ASet c a NE.NonEmpty)
selectGreedy parray = uniform . NE.filter ((pMax ==) . prediction) $ parray
  where
    pMax = NE.last . NE.sort $ prediction <$> parray
