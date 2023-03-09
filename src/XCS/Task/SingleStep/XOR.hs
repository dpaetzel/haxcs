{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Task.SingleStep.XOR where


import CustomPrelude


import XCS.Conf
import qualified XCS.Interface.BitString as BS
import qualified XCS.Population as PC
import XCS.Storage.Map (Storage)
import XCS.State


type Observation = BS.Observation 2


type Condition = BS.Condition 2


type Action = BS.Action 1


conf :: NonNegative -> Conf
conf steps =
  butz2001 (positive 120) 0.01 (positive 2) (> steps) & thetaGA .~ nonNegative 100


initialState :: State Storage Condition Action
initialState =
  State
    { population = PC.empty
    , time = 0
    , ep = nonNegative 0
    }


xor' :: Observation -> Action
xor' v = BS.singleton (BS.or v && not (BS.and v))
