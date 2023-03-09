{-|
Module      : XCS.Classifier.Rule
Description : Classifier rule type definition
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

A 'XCS.Classifier.Classifier''s rule.
-}


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module XCS.Classifier.Rule
  ( -- * Rule type
    Rule(..)
  -- * Lenses
  , c
  , a
  )
where



import CustomPrelude


{-|
A rule proposes an action to be carried out when the rule's condition matches
the current situation.

Access to the fields is provided by the corresponding lenses.
-}
data Rule c a = Rule
  { _c  :: c
  -- ^ The rule's condition.
  , _a  :: a
  -- ^ The action to be carried out, if the condition matches.
  }
  deriving (Eq, Ord)


makeLenses ''Rule


deriving instance (Show c, Show a) => Show (Rule c a)


instance (Arbitrary c, Arbitrary a) => Arbitrary (Rule c a) where
  arbitrary =
    Rule <$>
      arbitrary <*>
      arbitrary


instance (Pretty c, Pretty a) => Pretty (Rule c a) where
  pretty l =
    pretty (l^.c) <> text "?" <+>
    text "⇒ " <+> pretty (l^.a) <> text "!"
