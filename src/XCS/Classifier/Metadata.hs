{-|
Module      : XCS.Classifier.Metadata
Description : Classifier metadata type definition
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

A 'XCS.Classifier.Classifier''s metadata makes several statements about the
performance and usage history of the classifier's rule.
-}


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module XCS.Classifier.Metadata
  ( -- * Metadata and other types
    Prediction
  , Error
  , Fitness
  , Experience
  , Accuracy
  , Metadata(..)
  -- * Lenses and other accessors
  , x
  , tGA
  , p
  , n
  , f
  , e
  , aAvg
  , numMinusMinus
  )
where


import CustomPrelude


import Data.Text
import Text.Printf


{-|
Reward predictions are real numbers.
-}
type Prediction = Real


{-|
Error estimates are real numbers.
-}
type Error = Real


{-|
Fitness values are real numbers.
-}
type Fitness = Real


{-|
A classifier's experience is a positive integer.
-}
type Experience = NonNegative


{-|
Predictions are real numbers.
-}
type Accuracy = Real


{-|
A classifier's metadata consists of seven fields.

Access to the fields is provided by the corresponding lenses.
-}
data Metadata = Metadata
  { _p    :: Prediction
  -- ^ An estimation of the reward received after one application of the
  -- classifier's rule.
  , _e    :: Error
  -- ^ An estimation of the error of the prediction.
  , _f    :: Fitness
  -- ^ An estimation of the accuracy of the prediction,.
  , _x    :: Experience
  -- ^ How often the classifier's rule was applied (and correspondingly, how
  -- often the estimations were updated).
  , _tGA  :: Integer
  -- ^ The time step of the last time this classifier participated in the
  -- GA—unless the classifier did not yet take part in the GA, in which case
  -- |timeGA| is the time of the classifier's creation.
  , _aAvg :: Real
  -- ^ The average size of the action sets this classifier has been part of.
  , _n    :: Positive
  -- ^ The actual number of classifiers the classifier object stands for
  -- (multiple occurences of the exact same rule are combined into a single
  -- classifier so that no inconsistencies can occur in the rule set).
  }
  deriving (Show)
makeLenses ''Metadata
-- TODO Should tGA be of type Tick?
-- TODO Why reward and not payoff?


instance Arbitrary Metadata where
  arbitrary =
    Metadata <$>
      arbitrary `suchThat` (>= 0) <*>
      arbitrary `suchThat` (>= 0) <*>
      arbitrary `suchThat` (>= 0) <*>
      arbitrary <*>
      arbitrary `suchThat` (>= 0) <*>
      arbitrary `suchThat` (>= 0) <*>
      arbitrary


instance Pretty Metadata where
  pretty md =
    text "p:" <+> (text . dec2) (md^.p) <+>
    text "e:" <+> (text . dec2) (md^.e) <+>
    text "f:" <+> (text . dec2) (md^.f) <+>
    text "exp:" <+> pretty (md^.x) <+>
    text "ts:" <+> pretty (md^.tGA) <+>
    text "as:" <+> (text . dec2) (md^.aAvg) <+>
    text "n:" <+> pretty (md^.n)
    where
      dec2 = fromStrict . pack . printf "%.2f"


{-|
Decreases the metadata's numerosity by one. Numerosity can not be negative;
thus, this may result in 'Nothing'.
-}
numMinusMinus :: Metadata -> Maybe Metadata
numMinusMinus md = case md^.n >-> positive 1 of
  Just num' -> Just $ set n num' md
  Nothing -> Nothing
