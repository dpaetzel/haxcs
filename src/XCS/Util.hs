{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}


module XCS.Util where


import CustomPrelude


import Control.Monad.Random.Lazy
import Data.List (find, scanl')
import System.Random ()
import System.Random.Shuffle (shuffle')


-- | Selects a value from the given non-empty lists using a roulette' wheel based
-- on their respective “votes”. Any negative vote values are treated as if they
-- were zero; if the list is empty, return Nothing.
roulette :: (Num n, Ord n, Random n, MonadRandom r) => NonEmpty (a, n) -> r a
roulette xs@(x :| _) = do
  choicePoint <- getRandomR . (0,) . fromMaybe 0 . lastMay . fmap snd $ ss
  return . maybe (fst x) fst . find ((choicePoint <=) . snd) $ ss
  where
    ss = segments . fmap atLeastZero . toList $ xs
    atLeastZero (x, v)
      | v < 0     = (x, 0)
      | otherwise = (x, v)


-- | Selects a value from the given lists using a roulette' wheel based on their
-- respective “votes”. Any negative vote values are treated as if they were
-- zero; if the list is empty, return Nothing.
roulette' :: (Num n, Ord n, Random n, MonadRandom r) => [(a, n)] -> r (Maybe a)
roulette' xs = do
  choicePoint <- getRandomR . (0,) . fromMaybe 0 . lastMay . fmap snd $ ss
  return . fmap fst . find ((choicePoint <=) . snd) $ ss
  where
    ss = segments . fmap atLeastZero $ xs
    atLeastZero (x, v)
      | v < 0     = (x, 0)
      | otherwise = (x, v)


prop_rouletteNothing :: Property
prop_rouletteNothing =
  monadicIO $ do
    r <- run $ roulette' ([] :: [(Integer, Integer)])
    assert (isNothing r)


prop_rouletteSingle :: (Eq a, Random n, Num n, Ord n) => (a, n) -> Property
prop_rouletteSingle (x, v) =
  monadicIO $ do
    r <- run $ roulette' [(x, v)]
    assert (r == Just x)


prop_rouletteZeroProbability :: Eq a => a -> a -> Double -> Property
prop_rouletteZeroProbability x1 x2 v =
  v > 0 ==> monadicIO $ do
    r <- run $ roulette' [(x1, 0 :: Double), (x2, v)]
    assert (r == Just x2)


prop_rouletteAlwaysSelectOne :: (Eq a, Random n, Num n, Ord n) => [(a, n)] -> Property
prop_rouletteAlwaysSelectOne xs =
  any ((> 0) . snd) xs ==> monadicIO $ do
    r <- run $ roulette' xs
    assert (isJust r)


-- TODO tests whether this exactly resembles SELECT OFFSPRING in (Butz and Wilson, 2000).
tournament :: (Ord n, RandomGen g) => g -> Real -> [(a, n)] -> Maybe a
tournament _ _ [] = Nothing
tournament g tau xs =
  fmap fst . minimumByMay (comparing snd) . take (ceiling $ tau * toNum (length xs)) $ shuffle' xs (toNum $ length xs) g
  where
    minimumByMay c = headMay . sortBy c


segments :: Num n => [(a, n)] -> [(a, n)]
segments [] = []
segments (x : xs) = scanl' run x xs
  where
    run (_, vPrevious) (xNext, vNext) = (xNext, vPrevious + vNext)


prop_segmentsLength :: Num n => [(a, n)] -> Bool
prop_segmentsLength xs = length xs == (length . segments) xs


prop_segmentsSingle :: (a, Integer) -> Bool
prop_segmentsSingle = (nonNegative 1 ==) . length . segments . (: [])


return []
runTests :: IO Bool
runTests = $quickCheckAll
