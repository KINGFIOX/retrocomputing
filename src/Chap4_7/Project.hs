{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Chap4_7.Project where

import Clash.Prelude
import Simple

-- debounce ::
--   forall ps a dom.
--   (Eq a, NFDataX a) =>
--   (HiddenClockResetEnable dom, KnownNat (ClockDivider dom ps)) =>
--   SNat ps ->
--   a ->
--   Signal dom a ->
--   Signal dom a
-- debounce _ start this = regEn start stable this
--   where
--     counter = register (0 :: Index (ClockDivider dom ps)) counterNext
--     counterNext = mux (changed start this) 0 (moreIdx <$> counter)
--     stable = counterNext .== maxBound

topEntity ::
  Clock System ->
  Signal System (Active High) ->
  Signal System (Active High)
topEntity = withResetEnableGen board
  where
    board btn = toActive <$> led
      where
        btn' = debounce (SNat @(Milliseconds 5)) False $ fromActive <$> btn
        click = isRising False btn'
        led = regEn False click (not <$> led)
