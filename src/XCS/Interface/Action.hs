{-|
Module      : XCS.Interface.Action
Description : Action class
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Class of action types.
-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Interface.Action where


import CustomPrelude


import Control.Monad.Random.Class


{-|
Actions are just required to be comparable and implement a single function.
-}
class (Eq a, Ord a, Pretty a) => Action a where
  {-|
  Given a list of actions, return a random monad computation yielding an
  action not in that list. If the list contains all possible actions, no
  other action can be returned—yielding 'Nothing'.

  Implementations should probably use randomness if there is more than one
  actions allowed that is not in the supplied list.
  -}
  otherThan :: (MonadRandom m) => [a] -> m (Maybe a)


instance Action Bool where
  otherThan [] = Just . (0.5 <) <$> getRandomR (0, 1 :: Real)
  otherThan as
    | True  `elem` as && False `elem` as = return Nothing
    | True  `elem` as                    = return (Just False)
    | False `elem` as                    = return (Just True)
    | otherwise                          = return Nothing
