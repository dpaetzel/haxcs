{-|
Module      : XCS.Log
Description : 'Control.Monad.Writer.Lazy.Writer'-based logging
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Simple logging based on the lazy 'Control.Monad.Writer.Lazy.Writer' monad.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}


module XCS.Log where


import CustomPrelude


import Control.Monad.Writer (MonadWriter, tell)


import XCS.ASet (SystemPrediction)
import XCS.Classifier.Metadata (Error, Fitness, Prediction)
import XCS.Interface (Condition)
import XCS.Mode (Mode)
import qualified XCS.Population as P
import XCS.State (TimeStep)


{-|
Log entries either describe an episode or a single step of an episode.
-}
-- TODO Multiple-constructors records are not recommended and should probably
-- not be used here either
data Entry
  = EpisodeEntry
  { _episode :: NonNegative
  , _ret     :: Reward
  }
  | StepEntry
  { _time          :: TimeStep
  , _episode       :: NonNegative
  , _avgFitness    :: Fitness
  , _avgError      :: Error
  , _reward        :: Reward
  , _nMicro        :: NonNegative
  , _nMacro        :: NonNegative
  , _avgGenerality :: Real
  , _mode          :: Mode
  , _pSystem       :: SystemPrediction
  }
  deriving (Show)
makeLenses ''Entry


{-|
A log is a list of log entries.
-}
type Log = [Entry]


{-|
Appends an 'EpisodeEntry' to the 'Log'.
-}
logEpisode
  :: (MonadWriter Log m)
  =>  NonNegative
  -- ^ episode
  -> Reward
  -> m ()
logEpisode ep r =
  tell
    [ EpisodeEntry
      { _episode = ep
      , _ret     = r
      }
    ]


{-|
Appends a 'StepEntry' to the 'Log'.
-}
logStep
  :: (Condition c, P.Storage c a stor, MonadWriter Log m)
  => P.Population stor c a
  -> TimeStep
  -> NonNegative
  -- ^ episode
  -> Prediction
  -> Mode
  -> Reward
  -> m ()
logStep pop t ep pSystem mod r =
  tell
    [ StepEntry
      { _time          = t
      , _episode       = ep
      , _avgFitness    = P.avgFitness pop
      , _avgError      = P.avgError pop
      , _reward        = r
      , _nMicro        = P.nMicro pop
      , _nMacro        = P.nMacro pop
      , _avgGenerality = P.avgGenerality pop
      , _mode          = mod
      , _pSystem       = pSystem
      }
    ]
