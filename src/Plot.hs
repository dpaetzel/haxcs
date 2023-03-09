{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-type-defaults #-}


module Plot where


import CustomPrelude


import           Control.Arrow                ((&&&))
import           Data.Function                (on)
import           Data.List                    (groupBy)
import           Data.Csv                     as CSV
import qualified Graphics.EasyPlot            as P
import           System.Random                (Random)


import           XCS.Conf            (Conf, epsilon0)
import           XCS.Interface       (Detector, Mutator)
import qualified XCS.Log as Log
import           XCS.Mode
import           XCS.Population      (Storage)
import qualified XCS.Population      as P
import           XCS.State
import           XCS.Environment.Function (runFunctionEnvironment)


printPopulations
  :: (Foldable t, Functor t, MonadIO m, Pretty (stor c a), Show a1,
      Storage c a stor)
  => t (a1, State stor c a) -> m ()
printPopulations finalStates =
  sequence_ . flip fmap finalStates $ (\(s, st) -> do
    putStrLn (show s :: Text)
    putStrLn . displayT . renderCompact . pretty . population $ st)


csvPopulations
  :: (Detector o c, Mutator o c a, Random o, Storage c a stor, ToField a,
      ToField c)
  => [Int] -> Conf -> State stor c a -> (o -> a) -> IO ()
csvPopulations ss conf init f = do
  let runs = fmap (\s -> (s, runFunctionEnvironment conf s init f)) ss
  let finalStates = fmap (second fst) runs

  sequence_ . flip fmap finalStates $ (\(s, st) -> do
    putStrLn (show s :: Text)
    putStrLn . P.csv . population $ st)


plot
  :: (Detector o c, Mutator o c a, Random o, Storage c a stor)
  => [Int] -> Conf -> State stor c a -> (o -> a) -> IO ()
plot ss conf init f = do
  -- TODO use n to determine the number of ticks
  let runs = fmap (\s -> (show s, runFunctionEnvironment conf s init f)) ss
  let logs = fmap (second snd) runs :: [([Char], Log.Log)]

  let performancePlots = fmap (plotLabeled . second performance) logs
  let performanceMeanPlot = curry plotLabeled "performance" . meanRuns . fmap (second performance) $ logs
  let systemErrorPlots = fmap (plotLabeled . second systemError) logs
  let systemErrorMeanPlot = curry plotLabeled "error" . meanRuns . fmap (second systemError) $ logs
  let numMicroClassifiersPlots = fmap (plotLabeled . second numMicroClassifiers) logs
  let numMacroClassifiersPlots = fmap (plotLabeled . second numMacroClassifiers) logs
  let generalityPlots = fmap (plotLabeled . second generality) logs

  _ <- P.plot (P.PDF "performance.pdf") performancePlots
  _ <- P.plot (P.PDF "performanceMean.pdf") performanceMeanPlot
  _ <- P.plot (P.PDF "error.pdf") systemErrorPlots
  _ <- P.plot (P.PDF "errorMean.pdf") [systemErrorMeanPlot, e0Plot]
  _ <- P.plot (P.PDF "numMicroClassifiers.pdf") numMicroClassifiersPlots
  _ <- P.plot (P.PDF "numMacroClassifiers.pdf") numMacroClassifiersPlots
  _ <- P.plot (P.PDF "generality.pdf") generalityPlots

  return()

  where
    e0Plot = P.Gnuplot2D [ P.Title "e0" ] [] (show $ conf^.epsilon0)


plotLabeled :: ([Char], [(x, y)]) -> P.Graph2D x y
plotLabeled (s, dat) = P.Data2D [ P.Title s, P.Style P.Lines ] [] dat


plotBars :: [Char] -> [(x, y)] -> [P.Graph2D x y]
plotBars s dat = ($ dat) <$>
  [ P.Data2D [ P.Title s, P.Color P.Blue, P.Style P.Impulses ] []
  , P.Data2D [ P.Title "", P.Color P.Blue, P.Style P.Points ] []
  ]


onlySteps :: [Log.Entry] -> [Log.Entry]
onlySteps [] = []
onlySteps (s@Log.StepEntry{} : entries) = s : onlySteps entries
onlySteps (_ : entries) = onlySteps entries


exploits :: [Log.Entry] -> [Log.Entry]
exploits = filter (maybe False (Exploiting ==) . preview Log.mode)


performance :: [Log.Entry] -> [(Real, Real)]
performance = zip [0..] . meanWindow (positive 50) (preview Log.reward) . exploits


-- | Given a list of multiple runs' plot data, calculates the arithmetic mean on
-- them. This might be inaccurate because of the joining and grouping,
-- especially when there are sample points missing on some data sets
-- (interpolation might be required then).
meanRuns :: (Ord x, Num x, Fractional y) => [([Char], [(x, y)])] -> [(x, y)]
meanRuns = fmap (xCoord &&& mean . fmap snd) . groupX . (snd =<<)
  -- NOTE: (snd =<<) == join . fmap snd
  where
    xCoord = maybe 0 fst . headMay
    groupX = groupBy ((==) `on` fst) . sortOn fst


-- | Given a list of log entries, transforms each of them using a function and
-- calculates the mean values over a window of a given size.
--
-- Entries for which the function returns 'Nothing' are discarded.
--
-- Note: Unlike 'meanWindow'', this discards the time of each log entry.
meanWindow
  :: (Fractional n)
  => Positive -> (Log.Entry -> Maybe n) -> [Log.Entry] -> [n]
meanWindow n f = fmap mean . windows n . mapMaybe f


-- | Given a list of log entries, transforms each of them using a function and
-- calculates the mean values over a window of a given size.
--
-- Entries for which the function returns 'Nothing' are discarded.
--
-- Note: Unlike 'meanWindow', this keeps the time of each log entry.
meanWindow'
  :: forall n m. (Num n, Fractional m)
  => Positive -> (Log.Entry -> Maybe m) -> [Log.Entry] -> [(n, m)]
meanWindow' n f = fmap (time &&& average) . windows n . mapMaybe mkTuple
  where
    -- TODO fall back to 0, this is problematic
    time = fromIntegral . fromMaybe 0 . lastMay . fmap fst
    average = mean . fmap snd
    mkTuple e = (,) <$> e^?Log.time <*> f e


-- | Gets all windows of size n in the given list.
windows :: Positive -> [a] -> [[a]]
windows _ [] = []
windows n xs@(_ : xs')
  -- only consider windows of size n
  | length v < weaken n = []
  | otherwise           = v : windows n xs'
  where
    v = take (toNum n) xs


-- | The mean of the given set of numbers.
mean :: Fractional n => [n] -> n
mean xs = sum xs / toNum (length xs)


-- -- | The system error as described by (Wilson, 1995).
systemError :: [Log.Entry] -> [(Real, Reward)]
systemError = zip [0..] . meanWindow (positive 50) systemErrorSingle . exploits
  where
    systemErrorSingle l = Just . abs $ Log._pSystem l - Log._reward l


numMicroClassifiers :: [Log.Entry] -> [(Real, Real)]
numMicroClassifiers = fmap (fromIntegral . Log._time &&& toNum . Log._nMicro) . onlySteps


numMacroClassifiers :: [Log.Entry] -> [(Real, Real)]
numMacroClassifiers =
  fmap (fromIntegral . Log._time &&& toNum . Log._nMacro) . onlySteps


generality :: [Log.Entry] -> [(Real, Real)]
generality = fmap (fromIntegral . Log._time &&& Log._avgGenerality) . onlySteps
