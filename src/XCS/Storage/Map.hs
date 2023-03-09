{-|
Module      : XCS.Storage.Map
Description : 'Data.Map.Strict.Map'-based implementation of classifier
              'XCS.Storage'
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

An implementation of classifier 'XCS.Storage' based on the hash table
implementation in 'Data.Map.Strict.Map'.
-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Storage.Map where


import CustomPrelude


import qualified Data.Map.Strict as M
import qualified Text.PrettyPrint.Leijen.Text as PP


import XCS.Classifier
import qualified XCS.Storage as Stor


{-|
Storage type based on the hash table implementation in 'Data.Map.Strict.Map'.
-}
newtype Storage c a = Storage (M.Map (Rule c a) Metadata)
  deriving (Show)


instance (Ord c, Ord a) => Stor.Storage c a Storage where
  empty = Storage M.empty
  size (Storage m) = nonNegative . fromIntegral . M.size $ m
  retrieve l (Storage m) = Classifier l <$> M.lookup l m
  filter f (Storage m) =
    mkCls . M.filterWithKey (\l md -> f (Classifier l md)) $ m
    where
      mkCls = fmap (uncurry Classifier) . M.assocs
  insert cl (Storage m) = Storage $ M.insertWith incN (cl^.rule) (cl^.md) m
    where
      -- keep the old metadata
      incN mdNew = over n (>+> (mdNew^.n))
  update f l (Storage m) = Storage $ M.update f l m
  foldr f a (Storage m) =
    M.foldrWithKey (\rule md acc -> f (Classifier rule md) acc) a m


instance (Ord c, Ord a, Pretty c, Pretty a) => Pretty (Storage c a) where
  pretty = Stor.foldr (\cl doc -> pretty cl PP.<$> doc) mempty


instance
  (Ord c, Ord a, Arbitrary c, Arbitrary a)
  => Arbitrary (Storage c a) where
  arbitrary = fmap (Storage . M.fromList . fmap ((^.rule) &&& (^.md))) arbitrary
