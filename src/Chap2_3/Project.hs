-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Chap2_3.Project where

import Clash.Prelude
import Clash.Annotations.TH (makeTopEntity)

-- System 是 clash 预定义的一个时钟域

topEntity
  :: "BTN" ::: Signal System Bit
  -> "LED" ::: Signal System Bit
topEntity = id

makeTopEntity 'topEntity
