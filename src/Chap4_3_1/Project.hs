{-# LANGUAGE NumericUnderscores #-}

module Chap4_3_1.Project where

import Clash.Explicit.Prelude

type HzToPeriod (freq :: Nat) = 1_000_000_000_000 `Div` freq
type ClockDivider dom ps = ps `Div` DomainPeriod dom

blinkingSecond ::
  forall dom.
  (KnownDomain dom) =>
  (1 <= DomainPeriod dom, KnownNat (DomainPeriod dom)) => -- 时钟周期 >= 1
  (1 <= HzToPeriod 1 `Div` DomainPeriod dom) => -- 1s / 震动一次的时间(ps) = 时钟频率, 这个不等式就是在约束: 时钟频率 >= 1
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bit
blinkingSecond clk rst en = msb <$> r
  where
    -- ClockDivider dom (HztoPeriod 1) 其实就是: 时钟频率
    r :: Signal dom (Unsigned (CLog 2 (ClockDivider dom (HzToPeriod 1))))
    r = register clk rst en 0 $ mux (r .<. limit) (r + 1) 0

    limit = snatToNum (SNat @(ClockDivider dom (HzToPeriod 1)))

-- 这个版本的问题就是：占空比没法确定

-- 下面引出 Index 这个 funtor

-- >>> [minBound .. maxBound] :: [Index 14]
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13]

-- >>> (10 :: Index 14) + 3
-- 13

-- >>> (10 :: Index 14) + 4
-- X: Clash.Sized.Index: result 14 is out of bounds: [0..13]

-- >>> succ (10 :: Index 14)
-- 11

-- >>> succ (13 :: Index 14)
-- X: Clash.Sized.Index: result 14 is out of bounds: [0..13]
