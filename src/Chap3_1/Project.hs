-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Chap3_1.Project where

import Clash.Prelude

-- System 是 clash 预定义的一个时钟域

topEntity ::
  "BTN"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        ) ->
  "LED"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        )
topEntity (btn1, btn2) = (both, either')
  where
    both = (.&.) <$> btn1 <*> btn2
    either' = (.|.) <$> btn1 <*> btn2
