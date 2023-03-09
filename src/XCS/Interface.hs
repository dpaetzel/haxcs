{-|
Module      : XCS.Interface
Description : Problem interface
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

The interface to implement for a problem that is to be solved by XCS.
-}


{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}


module XCS.Interface
( module XCS.Interface.Action
, module XCS.Interface.Condition
, module XCS.Interface.Detector
, module XCS.Interface.Mutator
)
where


import CustomPrelude


import XCS.Interface.Action
import XCS.Interface.Condition
import XCS.Interface.Detector
import XCS.Interface.Mutator
