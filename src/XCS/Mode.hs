{-|
Module      : XCS.Mode
Description : Tags for exploration and exploitation
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Sum type to mark whether an experience has happened during exploitation or
exploration. Also, functions choosing one or the other.
-}


{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Mode where


import CustomPrelude


import Control.Monad.Random


{-|
Updates only take place in exploring steps; therefore we need to distinguish
between two modes.
-}
data Mode = Exploring | Exploiting
  deriving (Eq, Show)


{-|
Chooses whether to explore or exploit based on the given probability for
exploring.
-}
mode :: (MonadRandom m) => Probability -> m Mode
mode pExplr = mod . (< pExplr) <$> getRandomR (0 :: Double, 1 :: Double)
  where
    mod True = Exploring
    mod False = Exploiting
