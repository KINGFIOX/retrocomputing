-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Chap3_3.Project where

import Clash.Prelude

-- System 是 clash 预定义的一个时钟域

topEntity
  :: "SWITCHES" ::: Signal System (Vec 2 Bit)
  -> "LEDS" ::: Signal System (Vec 4 Bit)
topEntity switches = fmap (map boolToBit) . bundle $ both :> either' :> onlyOne :> onlyTheSecond :> Nil
  where
    sw1 :> sw2 :> Nil = unbundle $ map bitToBool <$> switches

    both = sw1 .&&. sw2
    either' = sw1 .||. sw2
    onlyOne = sw1 ./=. sw2
    onlyTheSecond = (not <$> sw1) .&&. sw2
