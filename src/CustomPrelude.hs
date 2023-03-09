{-|
Module      : CustomPrelude
Description : Custom prelude for this project
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Customized prelude based on 'Protolude'.
-}


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}


{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module CustomPrelude
( module X
, Real
, Reward
, Probability
, count
, length
, shuffleM
, untilM
, iterateUntilM
, iterateUntilM'
, prettify
)
where


import Protolude as X hiding (Real, State, exp, interact, length, log)
import qualified Protolude


import qualified Data.Csv                     as CSV
import           Text.PrettyPrint.Leijen.Text as X
  ( Pretty
  , (<+>)
  , char
  , displayT
  , pretty
  , renderOneLine
  , renderCompact
  , text
  )
import           Test.QuickCheck              as X hiding
  ( NonNegative
  , Positive
  , Result
  , (.&.)
  , cover
  )
import           Test.QuickCheck.Monadic      as X


import           Control.Arrow                as X ((&&&))
import           Control.Lens                 as X hiding
  -- these are in Protolude already
  ( (<.>)
  , Strict
  , elements
  , from
  , pre
  , to
  , uncons
  , unsnoc
  -- these are defined by haxcs
  , matching
  )
import           Control.Monad.Random.Class
import           Data.List.NonEmpty           as X (NonEmpty(..))
import qualified Data.List.NonEmpty           as NE
import qualified System.Random.Shuffle        as S


import           Naturals                     as X


-- | Reals are implemented using double-precision floating-point numbers.
type Real = Double


-- | Rewards are reals.
type Reward = Real


-- | Probabilities are reals—for now.
type Probability = Real


{-|
'Protolude.length' but with better types.
-}
length :: Foldable t => t a -> NonNegative
length = nonNegative . fromIntegral . Protolude.length


-- | /O(n)/ Counts the number of elements in the list that satisfy the given predicate.
count :: (a -> Bool) -> [a] -> NonNegative
count f = length . filter f


instance (Pretty x) => Pretty (NonEmpty x) where
  pretty = pretty . NE.toList


-- | Shuffles a non-empty list.
shuffleM :: (MonadRandom m) => NonEmpty a -> m (NonEmpty a)
shuffleM xs = do
  i <- getRandomR (0, NE.length xs - 1)
  let x = xs NE.!! i
  xs' <- S.shuffleM $ deleteI i xs
  return $ x :| xs'
  where
    deleteI i xs = fst (NE.splitAt (i - 1) xs) ++ snd (NE.splitAt i xs)


{-|
If the predicate resulting from evaluating the monadic Boolean value is true,
runs the given monadic computation.
-}
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM p f = do
  p' <- p
  when p' f



-- | Does something until a monadic predicate is true; always evaluates the
-- predicate first.
untilM :: (Monad m) => m Bool -> m () -> m ()
untilM p !f = do
  p' <- p
  unless p' $ do
    f
    untilM p f


-- -- | Performs a monadic action iteratively until a monadic predicate is true;
-- -- always evaluates the predicate first. Returns a (reverse) list of the results
-- -- after each step.
-- iterateUntilM' :: (Monad m) => m Bool -> (a -> m a) -> a -> m [a]
-- iterateUntilM' p f v = do
--   p' <- p
--   if p'
--      then return [v]
--      else do
--        traceShow "blu" $ return ()
--        v' <- f v
--        (v :) <$> iterateUntilM' p f v'


{-|
Performs a monadic action iteratively until a monadic predicate is true.
When the predicate is true, the monadic action will be performed one last time.
-}
iterateUntilM' :: (Monad m) => m Bool -> (a -> m a) -> a -> m [a]
iterateUntilM' p f v = do
  p' <- p
  if p'
     then (:[]) <$> f v
     else do
       v' <- f v
       (v' :) <$> iterateUntilM' p f v'


-- | Performs a monadic action iteratively until a monadic predicate is true;
-- always evaluates the predicate first. Returns the result of the last step.
iterateUntilM :: (Monad m) => m Bool -> (a -> m a) -> a -> m a
iterateUntilM p f v = do
  p' <- p
  if p'
     then f v
     else do
       v' <- f v
       iterateUntilM p f v'


instance CSV.ToField Bool where
  toField True = CSV.toField 'T'
  toField False = CSV.toField 'F'


prettify :: (Pretty x) => x -> LText
prettify = displayT . renderOneLine . pretty
