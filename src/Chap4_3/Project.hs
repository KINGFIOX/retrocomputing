{-# LANGUAGE NumericUnderscores #-}

module Chap4_3.Project where

import Clash.Explicit.Prelude

-- dom 是一个类型参数
-- DomainPeriod 是一个函数，可以知道: dom 晶振一次的时间(单位:ps)，比方说 50MHz -> 20ns -> 20_000 ps
-- 1s == 1_000_000_000_000 ps
-- 其实这个 SecondPeriods 的作用就是：提取出这个 dom 的频率
type SecondPeriods dom = 1_000_000_000_000 `Div` DomainPeriod dom

blinkingSecond ::
  forall dom.
  (KnownDomain dom) => -- 限制这个时钟域必须是已知的（type-level）
  (1 <= DomainPeriod dom, KnownNat (DomainPeriod dom)) => -- 1 <= DomainPeriod dom 是一个不等式. KnownNat (DomainPeriod dom) 是在约束频率要求是已知的自然数
  (1 <= 1_000_000_000_000 `Div` (DomainPeriod dom)) => -- 这是要求：时钟频率 >= 1Hz
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bit
blinkingSecond clk rst en = msb <$> r
  where
    r :: Signal dom (Unsigned (CLog 2 (SecondPeriods dom))) -- CLog: ceiling log（向上取整）
    r = register clk rst en 0 (r + 1)

-- 这个版本有问题: 就是他循环一次的时间是: (CLog 2 (SecondPeriods dom)), 这个时间不是严格的 1s, 最高位为 1 的时间(占空比)为 50%

topEntity ::
  "CLK" ::: Clock System ->
  "LED" ::: Signal System Bit
topEntity clk = blinkingSecond clk resetGen enableGen
