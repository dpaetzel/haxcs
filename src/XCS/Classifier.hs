{-|
Module      : XCS.Classifier
Description : Classifier type definition
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Classifiers are rules associated with metadata (e. g. about their accuracy).
-}


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module XCS.Classifier
  ( module XCS.Classifier.Metadata
  , module XCS.Classifier.Rule
  -- TODO do not export the constructor
  -- * Classifier type
  , Classifier(Classifier)
  , classifier
  -- * Lenses and other accessors
  , rule
  , md
  , num'
  -- * Logic
  , subsumes
  , update
  , runTests
  )
where


import CustomPrelude


import qualified Data.Csv as CSV


import XCS.Classifier.Rule
import XCS.Classifier.Metadata
import XCS.Interface


{-|
Given a condition type @c@ and an action type @a@, a classifier consists of a
rule and metadata about that rule.

Access to the fields is provided by the 'rule' and 'md' lenses.
-}
data Classifier c a = Classifier
  { _rule  :: Rule c a
  , _md    :: Metadata
  }
makeLenses ''Classifier


instance (Eq c, Eq a) => Eq (Classifier c a) where
  (==) = (==) `on` (^.rule)


deriving instance (Show c, Show a) => Show (Classifier c a)


instance (Arbitrary c, Arbitrary a) => Arbitrary (Classifier c a) where
  arbitrary =
    Classifier <$>
      arbitrary <*>
      arbitrary


instance (Pretty c, Pretty a) => Pretty (Classifier c a) where
  pretty cl =
    pretty (cl^.rule) <+>
    pretty (cl^.md)


instance (CSV.ToField c, CSV.ToField a) => CSV.ToRecord (Classifier c a) where
  toRecord cl = CSV.record $ ($ cl) <$>
    [ CSV.toField . (^.rule.c)
    , CSV.toField . (^.rule.a)
    , CSV.toField . (^.md.p)
    , CSV.toField . (^.md.e)
    , CSV.toField . (^.md.f)
    , CSV.toField . (^.md.x)
    , CSV.toField . (^.md.tGA)
    , CSV.toField . (^.md.aAvg)
    , CSV.toField . (^.md.n)
    ]


{-|
Just a 'Num' version of @^.md.num@.
-}
num' :: (Num n) => Classifier c a -> n
num' = toNum . (^.md.n)


{-|
Creates a new classifier with the default metadata values from the
configuration.

Note that since the 'XCS.Classifier.Metadata.tGA' field has to be set to the
time of the classifier's creation, it has to be supplied as a parameter to this
function (Butz and Wilson, 2000).
-}
classifier
  :: Real
  -- ^ Initial prediction value.
  -> Real
  -- ^ Initial prediction error value.
  -> Real
  -- ^ Initial fitness value.
  -> c
  -- ^ Condition.
  -> a
  -- ^ Action.
  -> Integer
  -- ^ The classifier's creation time; the 'tGA' field is set to this value.
  -> Classifier c a
classifier pI eI fI c a t =
  Classifier
  { _rule = Rule
    { _c = c
    , _a = a
    }
  , _md = Metadata
    { _p = pI
    , _e = eI
    , _f = fI
    , _x = nonNegative 0
    , _tGA = t
    , _aAvg = 1
    , _n = positive 1
    }
  }


{-|
Returns whether one classifier subsumes the other.

Corresponds to DOES SUBSUME in (Butz and Wilson, 2000).
-}
-- DOES SUBSUME (clSub, clTOS):
-- if (a clSub == a clTOS)
--   if (clSub COULD SUBSUME)
--     if (clSub IS MORE GENERAL than clTOS)
--       return true
-- return false
--
-- COULD SUBSUME (cl):
-- if (exp cl > thetaSub)
--   if (e cl < epsilon0)
--     return true
-- return false
subsumes
  :: (Condition c, Eq a)
  => NonNegative
  -- ^ 'XCS.Conf.thetaSub'.
  -> Real
  -- ^ 'XCS.Conf.epsilon0'.
  -> Classifier c a
  -> Classifier c a
  -> Bool
subsumes thetaSub epsilon0 cl1 cl2 =
  cl1^.rule.a == cl2^.rule.a &&
    couldSubsume thetaSub epsilon0 cl1 &&
    cl1^.rule.c `generalizes` cl2^.rule.c


couldSubsume :: NonNegative -> Real -> Classifier c a -> Bool
couldSubsume thetaSub epsilon0 cl = cl^.md.x > thetaSub && cl^.md.e < epsilon0


-- TODO rather update just the metadata in a nice way?
-- TODO more comments about parameters
{-|
Updates the given set of classifiers according to the supplied reward.

Corresponds to UPDATE SET in (Butz and Wilson, 2000).
-}
update
  :: Real
  -- ^ 'XCS.Conf.beta'.
  -> Real
  -- ^ 'XCS.Conf.epsilon0'.
  -> Real
  -- ^ 'XCS.Conf.alpha'.
  -> Real
  -- ^ 'XCS.Conf.nu'.
  -> Reward
  -- ^ The reward to incorporate.
  -> [Classifier c a]
  -- ^ Set of classifiers to update.
  -> [Classifier c a]
update beta epsilon0 alpha nu r = updateOnlyF beta epsilon0 alpha nu . updateButNotF beta r


{-|
Updates the given set of classifiers---but not their fitness.
-}
updateButNotF
  :: Real
  -- ^ 'XCS.Conf.beta'.
  -> Reward
  -- ^ The reward to incorporate.
  -> [Classifier c a]
  -- ^ Set of classifiers to update.
  -> [Classifier c a]
updateButNotF beta r cls = fmap update cls
  where
    update cl =
      let
        x' = cl^.md.x >=+> positive 1
        widrowHoff = max (1.0 / toNum x') beta
        p' = cl^.md.p + (r - cl^.md.p) * widrowHoff
        e' = cl^.md.e + (abs (r - p') - cl^.md.e) * widrowHoff
        aAvg' =
          cl^.md.aAvg + ((sum . map num' $ cls) - cl^.md.aAvg) * widrowHoff
      in
        over md (\md -> md { _x = weaken x', _p = p', _e = e', _aAvg = aAvg'}) cl


{-|
Updates the fitness of the classifiers considering their other properties.
-}
-- TODO not nice: updateOnlyF is not idempotent in (^.md.f) but there is no
-- parameter that “shows” that (as is in updateButNotF)!
-- (updateOnlyF conf cls) /= (updateOnlyF conf (updateOnlyF conf cls))
updateOnlyF
  :: Real
  -- ^ 'XCS.Conf.beta'.
  -> Real
  -- ^ 'XCS.Conf.epsilon0'.
  -> Real
  -- ^ 'XCS.Conf.alpha'.
  -> Real
  -- ^ 'XCS.Conf.nu'.
  -> [Classifier c a]
  -- ^ Set of classifiers to update.
  -> [Classifier c a]
updateOnlyF beta epsilon0 alpha nu cls = map update' cls
  where
    update' cl =
      let
        f' = cl^.md.f +
          beta * (accuracy cl * num' cl / accuracySum - cl^.md.f)
      in
        set (md.f) f' cl
    accuracy cl =
      if cl^.md.e < epsilon0 then
        1.0
      else
        alpha * (cl^.md.e / epsilon0) ** (- nu)
    accuracySum =
      sum . map (\cl -> accuracy cl * num' cl) $ cls


return []
runTests :: IO Bool
runTests = $quickCheckAll
