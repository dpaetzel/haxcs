{-|
Module      : XCS.Interface.BitString
Description : Standard XCS bit string interface
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Implementation of the standard XCS bit string interface.

It seems like there are mainly two options to implement this in a properly typed
way:

- [non-structurally (but performant), based on typelits](http://hackage.haskell.org/package/vector-sized-0.6.1.0/docs/Data-Vector-Sized.html)

- [structurally, based on peano-numbers](http://hackage.haskell.org/package/type-combinators-0.2.4.3/docs/Data-Type-Vector.html)

We decided to use the first for performance reasons.
-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


module XCS.Interface.BitString
  ( module XCS.Interface.BitString
  , V.and
  , V.or
  , V.indexed
  , V.singleton
  , V.splitAt
  , V.unsafeIndex
  , V.zipWithM
  )
where


import CustomPrelude


import Control.Monad.Random
import qualified Data.Csv as CSV
import Data.List ((\\))
import qualified Data.Vector.Sized as V
import qualified Data.Finite.Internal as DFI
import GHC.TypeLits
import qualified Text.PrettyPrint.Leijen.Text as PP


import qualified XCS.Interface as I
import XCS.Classifier.Rule (Rule(Rule))


instance (Pretty x) => Pretty (V.Vector n x) where
  pretty = V.foldl (\acc x -> acc <> pretty x) PP.empty


instance (Pretty x) => CSV.ToField (V.Vector n x) where
  toField = CSV.toField . displayT . renderOneLine . pretty


instance (CoArbitrary x, KnownNat n, Arbitrary x) =>
  Arbitrary (V.Vector n x)
  where
  arbitrary = V.iterateN <$> arbitrary <*> arbitrary


{-|
Observations are simple bit vectors, that is, vectors of Booleans.
-}
type Observation n = V.Vector n Bool


{-|
Conditions are simple trit vectors, that is, vectors of ternary digits.
-}
type Condition n = V.Vector n (Wildcarded Bool)


{-|
Actions are simple bit vectors, that is, vectors of Booleans.
-}
type Action n = V.Vector n Bool


{-|
Just like 'Maybe', augments the given type with a wildcard option.

We don't use 'Maybe' because the semantics are different: A value of 'Wildcard'
represents “all possible values at once” whereas a value of 'Nothing' usually
represents “no value at all”.
-}
data Wildcarded x = Wildcard | Exactly x
  deriving (Eq, Ord, Show)


instance (Eq x, Random x) => Random (Wildcarded x) where
  randomR (lo, hi) g
    | lo == hi   = (lo, g)
    | otherwise  = random g
  random g =
    let
      (wild, g') = random g
      (v, g'') = random g'
    in
      if wild then
        (Wildcard, g')
      else
        (Exactly v, g'')


instance (Eq x, Arbitrary x, Random x) => Arbitrary (Wildcarded x) where
  arbitrary = (Wildcard,) . Exactly <$> arbitrary >>= choose


instance Pretty x => Pretty (Wildcarded x) where
  pretty (Exactly x) = pretty x
  pretty Wildcard = text "#"


instance {-# OVERLAPPING #-} Pretty (Wildcarded Bool) where
  pretty Wildcard        = char '#'
  pretty (Exactly True)  = char 'T'
  pretty (Exactly False) = char 'F'


-- NOTE KnownNat was added merely b/c the compiler told us to do so
instance (KnownNat n, (k + 1) ~ n) => Random (Observation n) where
  randomR (v1, v2) g =
    let
      vecR = V.zipWith (,) v1 v2
    in
      first ((V.//) v1 . zip [0..]) $
        V.foldr (\r (xs, g) ->
                    let (x, g') = randomR r g
                    in (x : xs, g')) ([], g) vecR
  random g =
    let
      vec = V.iterateN f (random g)
      f (_, g) = random g
    in
      (V.map fst vec, snd . V.last $ vec)


instance I.Condition (Condition n) where
  generality = toNum . count (== Wildcard) . V.toList
  generalizes c1 c2 =
    I.generality c1 > I.generality c2 && V.and (V.zipWith f c1 c2)
    where
      f x y = not $ x /= Wildcard && x /= y
  crossover1 c1 c2 = do
    let l = toNum $ min (length c1) (length c2) :: Double
    x <- (l *) <$> getRandom
    let c1' = V.izipWith (f x) c1 c2
    let c2' = V.izipWith (f x) c2 c1
    return (c1', c2')
    where
      f x i v1 v2 = if fromIntegral (DFI.getFinite i) <= x then v1 else v2


instance (KnownNat n) => I.Action (Action n) where
  otherThan as = case all \\ as of
    [] -> return Nothing
    as' -> Just <$> uniform as'
    where
      all = V.mapM (const [True, False]) (V.replicate True :: Action n)


instance I.Detector (Observation n) (Condition n) where
  matchedBy o c = V.and $ V.zipWith f o c
    where
      f oe ce = Exactly oe == ce || ce == Wildcard
  coverCondition pWild = mapM coverBit
    where
      coverBit b =
        liftM3 bool
          (return $ Exactly b)
          (return Wildcard)
          ((< pWild) <$> getRandomR (0, 1))


instance (KnownNat n, KnownNat k)
  => I.Mutator (Observation n) (Condition n) (Action k) where
  mutate mu o (Rule c a) =
    return Rule `ap` mutatedCondition `ap` mutatedAction
    where
      mutatedCondition = V.zipWithM mutateC c o
      mutateC c@Wildcard o =
        bool c (Exactly o) <$> shouldMutate mu
      mutateC c _ =
        bool c Wildcard <$> shouldMutate mu
      mutatedAction = sequence $ mutateA <$> a
      mutateA a = bool (not a) a <$> shouldMutate mu


shouldMutate :: (MonadRandom r) => Probability -> r Bool
shouldMutate mu = (< mu) <$> getRandomR (0, 1)
