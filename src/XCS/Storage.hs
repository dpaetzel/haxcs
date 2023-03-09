{-|
Module      : XCS.Storage
Description : Class for classifier storage
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

A storage is a data structure for holding macro-classifiers that supports a
number of functions efficiently.

It is used as a backend to 'XCS.Population'.
-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Storage
  ( Storage
  , empty
  , size
  , retrieve
  , filter
  , insert
  , update
  , foldr
  )
where


import CustomPrelude hiding (empty, filter, foldr)


import XCS.Classifier (Classifier, Metadata, Rule)


{-|
Given a condition type @c@ and an action type @a@, a storage type @stor c a@ has
to support a number of functions.

A storage contains each macro-classifier at most once (see
'XCS.Classifier.Metadata._n').

The 'Ord' contexts might be required for all efficient implementations of
'retrieve'.
-}
class (Ord c, Ord a) => Storage c a stor where
  empty :: stor c a
  -- ^ An empty storage.
  size :: stor c a -> NonNegative
  -- ^ The number of macro-classifiers in the storage.
  retrieve :: Rule c a -> stor c a -> Maybe (Classifier c a)
  -- ^ If a classifier with the given 'XCS.Classifier.Rule' exists in the
  -- storage, returns it wrapped in a 'Data.Maybe.Just'—otherwise returns
  -- 'Data.Maybe.Nothing'.
  --
  -- TODO Is an efficient retrieve really needed?
  filter :: (Classifier c a -> Bool) -> stor c a -> [Classifier c a]
  -- ^ Returns The classifiers in the storage whose conditions match the given
  -- observation.
  insert :: Classifier c a -> stor c a -> stor c a
  -- ^ The storage but with the given classifier inserted. If a corresponding
  -- macro-classifier already exists in the storage, that classifier's
  -- numerosity is to be increased. Thus, the metadata of existing classifiers
  -- is kept while the metadata of new classifiers is discarded.
  update :: (Metadata -> Maybe Metadata) -> Rule c a -> stor c a -> stor c a
  -- ^ @update f l stor@ updates the metadata of the classifier with rule @l@ in
  -- @stor@ using @f@. If @f@ returns @Nothing@, the classifier is deleted.
  foldr :: (Classifier c a -> y -> y) -> y -> stor c a -> y
  -- ^ Uses the given right-associative operator to reduce the storage to a
  -- single value.
