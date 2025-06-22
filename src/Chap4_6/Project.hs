{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Chap4_6.Project where

import Clash.Prelude
import Simple

data OnOff on off
  = On (Index on)
  | Off (Index off)
  deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On {} = True
isOn Off {} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On x) = maybe (Off 0) On $ succIdx x
countOnOff (Off y) = maybe (On 0) Off $ succIdx y

blinkingSecond ::
  forall dom.
  (HiddenClockResetEnable dom, _) =>
  Signal dom Bit
blinkingSecond = boolToBit . isOn <$> r
  where
    r ::
      Signal
        dom
        ( OnOff
            (ClockDivider dom (Milliseconds 500))
            (ClockDivider dom (Milliseconds 500))
        )
    r = register (Off 0) $ countOnOff <$> r

topEntity ::
  "CLK" ::: Clock System ->
  "BTN" ::: Signal System (Active High) ->
  "LED" ::: Signal System (Active High)
topEntity = withResetEnableGen board
  where
    board btn = toActive <$> led
      where
        btn' = fromActive <$> btn
        click = btn' .&&. (not <$> register False btn') -- register False btn' 延迟了一个周期，判断是否按下：上一个周期是低电平，下一个周期是高电平
        led = register False $ mux click (not <$> led) led
