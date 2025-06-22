{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Chap4_3_4.Project where

import Clash.Prelude

type HzToPeriod (freq :: Nat) = 1_000_000_000_000 `Div` freq

type ClockDivider dom ps = ps `Div` DomainPeriod dom

type Seconds (s :: Nat) = Milliseconds ((*) 1_000 s)

type Milliseconds (ms :: Nat) = Microseconds ((*) 1_000 ms)

type Microseconds (us :: Nat) = Nanoseconds ((*) 1_000 us)

type Nanoseconds (ns :: Nat) = Picoseconds ((*) 1_000 ns)

type Picoseconds (ps :: Nat) = ps

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

blinkingSecond
  :: forall dom.  (HiddenClockResetEnable dom, _)
  => Signal dom Bit
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

createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity ::
  "CLK" ::: Clock Dom100 ->
  "LED" ::: Signal Dom100 Bit
topEntity clk = withClockResetEnable clk resetGen enableGen blinkingSecond
