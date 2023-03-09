{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}


{-# OPTIONS_GHC -Wno-type-defaults #-}


import CustomPrelude


-- import Control.Monad.Random.Lazy


-- import qualified XCS.MSet                as MSet
-- import qualified XCS                     as X
-- import qualified XCS.Classifier          as Cl
-- import qualified XCS.Config              as C
-- import qualified XCS.Interface.BitString as BS
-- import qualified XCS.Population          as P
-- import qualified XCS.Population.Map      as M
-- import qualified XCS.State               as S
-- import qualified XCS.Util                as U


-- TODO reinstate tests with proper coverage
-- prop_insertResultingSize :: (Ord c, Ord a) =>
--   C.Config -> S.State M.Population c a -> [Cl.Classifier c a] -> Property
-- prop_insertResultingSize conf init cls =
--   monadicIO $ do
--     g <- run getStdGen
--     assert $ evalRand (evalStateT (runReaderT test conf) init) g
--   where
--     test = do
--       nMax <- C.n <$> ask
--       n <- P.size <$> gets S.population
--       S.insert cls
--       n' <- P.size <$> gets S.population
--       return $ n' == weaken nMax || toNum n' == toNum n + (sum . fmap Cl.num') cls


-- -- TODO clean this up: S.prune nMax suffices?
-- -- prop_pruneResultingSize :: (Ord c, Ord a) =>
-- --   C.Config -> S.State M.Population c a -> Property
-- -- prop_pruneResultingSize conf init =
-- --   monadicIO $ do
-- --     g <- run getStdGen
-- --     assert $ evalRand (evalStateT (runReaderT test (setLowN conf)) init) g
-- --   where
-- --     setLowN conf = conf { C.n = positive 30 }
-- --     test = do
-- --       conf <- ask
-- --       let nMax = C.n conf
-- --       S.prune conf
-- --       n' <- P.size <$> gets S.population
-- --       return $ n' <= weaken nMax


-- prop_coveringClassifierSelectsSingleton :: C.Config -> BS.Observation 1 -> Property
-- prop_coveringClassifierSelectsSingleton conf o = monadicIO $ do
--   cl <- run (MSet.coveringClassifier conf 1 o
--              [ Cl.Classifier
--                (Cl.Rule (BS.singleton (BS.Exactly True)) True)
--                (Cl.Metadata 1 1 1 (nonNegative 1) 1 1 (positive 1))
--              ])
--   assert (isJust cl)


-- prop_missingActionSelectsSingleOption :: Property
-- prop_missingActionSelectsSingleOption = monadicIO $ do
--   a <- run (MSet.missingAction
--             [ Cl.Classifier
--               (Cl.Rule 1 True)
--               (Cl.Metadata 1 1 1 (nonNegative 1) 1 1 (positive 1))
--             ])
--   assert (a == Just False)


return []
main :: IO ()
main = -- do
  -- TODO add tests here
  -- _ <- U.runTests
  -- _ <- Cl.runTests
  -- _ <- X.runTests
  -- -- $verboseCheckAll
  -- _ <- $quickCheckAll
  return ()
