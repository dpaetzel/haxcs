{-|
Module      : Naturals
Description : Types for natural numbers
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Sometimes, 'Int's just aren't enough. These types allow to ensure the usage of
non-negative or positive integers at the type level.
-}


{-# LANGUAGE DeriveGeneric #-}


module Naturals
( Numable
, toNum
, sum'
, NonNegative()
, nonNegative
, Positive()
, positive
, weaken
, (>=+>=)
, (>+>=)
, (>=+>)
, (>+>)
, (>->)
)
where


import Data.Csv                     (ToField, toField)
import GHC.Generics (Generic)
import GHC.Stack
import Test.QuickCheck              (Arbitrary, CoArbitrary, arbitrary, suchThat)
import Text.PrettyPrint.Leijen.Text hiding ((<$>))


{-|
Types that can be converted to 'Num's but are not necessarily closed under more
than a sum operation (that is, they might not support subtraction etc.).
-}
class Numable a where
  toNum :: (Num b) => a -> b
  -- ^ Transforms the given 'Numable' to a 'Num'. This usually widens the type.
  sum' :: [a] -> a
  -- ^ Sums a list of 'Numable's.


{-|
Non-negative integers.
-}
newtype NonNegative = NonNegative Integer
  deriving (Eq, Generic, Ord, Show)


instance Numable NonNegative where
  toNum (NonNegative i) = fromIntegral i
  sum' = nonNegative . sum . fmap toNum


instance Arbitrary NonNegative where
  arbitrary = nonNegative <$> arbitrary `suchThat` (>= 0)


instance CoArbitrary NonNegative


instance Pretty NonNegative where
  pretty (NonNegative i) = pretty i


instance ToField NonNegative where
  toField (NonNegative i) = toField i


{-|
Unsafely constructs a non-negative integer.

Use this with care!
-}
nonNegative :: HasCallStack => Integer -> NonNegative
nonNegative i
  | i >= 0     = NonNegative i
  | otherwise  = error "cannot construct NonNegative from negative Integer"


{-|
Positive integers.
-}
newtype Positive = Positive Integer
  deriving (Eq, Generic, Ord, Show)


instance Numable Positive where
  toNum (Positive i) = fromIntegral i
  sum' = positive . sum . fmap toNum


instance Arbitrary Positive where
  arbitrary = positive <$> arbitrary `suchThat` (> 0)


instance CoArbitrary Positive


instance Pretty Positive where
  pretty (Positive i) = pretty i


instance ToField Positive where
  toField (Positive i) = toField i


{-|
Unsafely constructs a positive integer.

Use this with care!
-}
positive :: HasCallStack => Integer -> Positive
positive i
  | i > 0      = Positive i
  | otherwise  = error $ "cannot construct Positive from non-positive Integer " ++ show i


{-|
Weakens a positive integer to a non-negative integer.
-}
weaken :: Positive -> NonNegative
weaken (Positive i) = NonNegative i


{-|
Sums two non-negative integers.
-}
infixl 6 >=+>=
(>=+>=) :: NonNegative -> NonNegative -> NonNegative
NonNegative i1 >=+>= NonNegative i2 = NonNegative (i1 + i2)


{-|
Sums a positive and a non-negative integer.
-}
infixl 6 >+>=
(>+>=) :: Positive -> NonNegative -> Positive
Positive i1 >+>= NonNegative i2 = Positive (i1 + i2)


{-|
Sums a non-negative and a positive integer.
-}
infixl 6 >=+>
(>=+>) :: NonNegative -> Positive -> Positive
NonNegative i1 >=+> Positive i2 = Positive (i1 + i2)


{-|
Sums two non-negative integers.
-}
infixl 6 >+>
(>+>) :: Positive -> Positive -> Positive
Positive i1 >+> Positive i2 = Positive (i1 + i2)


{-|
Subtracts two positive integers yielding 'Nothing' if the result is no positive.
-}
infixl 6 >->
(>->) :: Positive -> Positive -> Maybe Positive
Positive i1 >-> Positive i2
  | i > 0        = Just . Positive $ i
  | otherwise  = Nothing
  where
    i = i1 - i2
