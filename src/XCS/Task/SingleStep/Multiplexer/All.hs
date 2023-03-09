{-|
Module      : XCS.Task.SingleStep.Multiplexer.All
Description : Auxiliary definitions of multiplexers
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Defines a number of multiplexers to facilitate using them.
-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Task.SingleStep.Multiplexer.All where


import CustomPrelude


import XCS.Conf
import XCS.State (State)
import XCS.Storage.Map (Storage)
import XCS.Task.SingleStep.Multiplexer


conf6 :: NonNegative -> Conf
conf6 steps = (conf steps) { _nMicroMax = positive 400 }


mux6 :: Observation 6 -> Action
mux6 = multiplex (Proxy :: Proxy 2)


initial6 :: State Storage (Condition 6) Action
initial6 = initialState


conf11 :: NonNegative -> Conf
conf11 = conf


mux11 :: Observation 11 -> Action
mux11 = multiplex (Proxy :: Proxy 3)


initial11 :: State Storage (Condition 11) Action
initial11 = initialState


conf135 :: NonNegative -> Conf
conf135 steps = (conf steps) { _nMicroMax = positive 2000 }


mux135 :: Observation 135 -> Action
mux135 = multiplex (Proxy :: Proxy 7)


initial135 :: State Storage (Condition 135) Action
initial135 = initialState
