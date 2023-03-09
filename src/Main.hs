{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Main where


import CustomPrelude hiding (option)


import           Options.Applicative
import           System.Random                (Random)


import           Plot
import           XCS.Interface     (Detector, Mutator)
import           XCS.Conf (Conf)
import           XCS.Population    (Storage)
import           XCS.State         (State)
import XCS.Task.SingleStep.Multiplexer.All
import qualified XCS.Task.SingleStep.XOR as XOR


data Problem = XOR | Multiplexer6 | Multiplexer11 | Multiplexer135
  deriving (Read)


data Options = Options
  -- TODO we should use NonNegative here and not nonNegative down below :)
  { steps :: Integer
  , runs :: Integer
  , problem :: Problem
  }


options :: Parser Options
options = Options
  <$> option auto
    ( long "steps"
    <> short 's'
    <> metavar "N"
    <> help "Number of steps to run"
    )
  <*> option auto
    ( long "runs"
    <> short 'r'
    <> metavar "M"
    <> help "Number of runs to execute"
    )
  <*> option auto
    ( long "problem"
    <> short 'p'
    <> metavar "P"
    <> help "Which problem to try to solve; one of XOR, Multiplexer6, Multiplexer11, Multiplexer135")


dealWithIt :: Options -> IO ()
dealWithIt options =
  case problem options of
    Multiplexer6 ->
      runPlot options
        (conf6 . nonNegative $ steps options) initial6 mux6
    Multiplexer11 ->
      runPlot options
        (conf11 . nonNegative $ steps options) initial11 mux11
    Multiplexer135 ->
      runPlot options
        (conf135 . nonNegative $ steps options) initial135 mux135
    XOR ->
      runPlot options
        (XOR.conf . nonNegative $ steps options) XOR.initialState XOR.xor'


runPlot
  :: (Detector o c, Mutator o c a, Pretty (stor c a), Random o,
      Storage c a stor)
  => Options -> Conf -> State stor c a -> (o -> a) -> IO ()
runPlot options = plot [0..(fromIntegral $ runs options - 1)]


main :: IO ()
main = execParser optionsWithHelp >>= dealWithIt
  where
    optionsWithHelp = info (helper <*> options)
      ( fullDesc
      <> progDesc "Perform an XCS run, creating graphs"
      <> header "haxcs - Haskell implementation and formalisation of XCS"
      )
