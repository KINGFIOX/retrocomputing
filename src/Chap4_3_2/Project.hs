{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Chap4_3_2.Project where

import Clash.Explicit.Prelude

type HzToPeriod (freq :: Nat) = 1_000_000_000_000 `Div` freq

type ClockDivider dom ps = ps `Div` DomainPeriod dom

data OnOff on off
  = On (Index on)
  | Off (Index off)
  deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On {} = True
isOn Off {} = False

succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x
  | x == maxBound = Nothing
  | otherwise = Just $ succ x

predIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
predIdx x
  | x == minBound = Nothing
  | otherwise = Just $ succ x

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On x) = maybe (Off 0) On $ succIdx x
countOnOff (Off y) = maybe (On 0) Off $ succIdx y

blinkingSecond ::
  forall dom.
  (KnownDomain dom, _) => -- 这个 _ 就是 PartialTypeSignatures
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bit
blinkingSecond clk rst en = boolToBit . isOn <$> r
  where
    r ::
      Signal
        dom
        ( OnOff
            (ClockDivider dom (500_000_000_000))
            (ClockDivider dom (500_000_000_000))
        )
    r = register clk rst en (Off 0) $ countOnOff <$> r

topEntity ::
  "CLK" ::: Clock System ->
  "LED" ::: Signal System Bit
topEntity clk = blinkingSecond clk resetGen enableGen

