{-|
Module      : XCS.Task.SingleStep.Multiplexer
Description : l-multiplexer task
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

An abstract implementation of the @l@-multiplexer for any valid @l@. An @l@ is
valid if there exists a @k >= 1@ such that @l = k + 2^k@.
-}


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


module XCS.Task.SingleStep.Multiplexer
  ( module XCS.Task.SingleStep.Multiplexer
  )
where


import CustomPrelude


import GHC.TypeLits


import XCS.Conf
import qualified XCS.Interface.BitString as BS
import qualified XCS.Population as PC
import XCS.Storage.Map (Storage)
import XCS.State


type Observation l = BS.Observation l


type Condition l = BS.Condition l


type Action = BS.Action 1


conf :: NonNegative -> Conf
conf steps =
  (butz2001 (positive 800) 0.01 (positive 2) (> steps))
    { _beta = 0.2
    , _gamma = 0.71
    , _thetaGA = nonNegative 25
    , _epsilon0 = 0.01
    , _alpha = 0.1
    , _chi = 0.8
    , _mu = 0.04
    , _delta = 0.1
    , _pWild = 0.33
    , _pI = 0.1
    , _eI = 0
    , _fI = 0.1

    , _pExplr = 0.5

    , _thetaDel = nonNegative 20
    , _doGASubsumption = True
    , _doASetSubsumption = False
    }


initialState :: State Storage (Condition l) Action
initialState =
  State
    { population = PC.empty
    , time = 0
    , ep = nonNegative 0
    }


multiplex
  :: forall l k p. (KnownNat l, KnownNat k, 1 <= k, l ~ (k + 2^k))
  => p k -> Observation l -> Action
multiplex _ v =
  let
    (addressBits, dataBits) = BS.splitAt v :: (Observation k, Observation (2^k))
    f (i, True) = 2^i
    f (_, False) = 0
    address = sum . map f $ BS.indexed addressBits
  in
    -- TODO calculate the address at the type level instead of using `unsafeIndex`
    -- V.unsafeIndex dataBits address
    BS.singleton (BS.unsafeIndex dataBits address)
