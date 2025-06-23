{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Chap5_2_2.Project where

import Clash.Prelude
import qualified Data.List as L
import Simple

-- just for debugging with ascii text
showSS :: Vec 7 Bool -> String
showSS (a :> b :> c :> d :> e :> f :> g :> Nil) = unlines . L.concat $ [L.replicate 1 $ horiz a, L.replicate 3 $ vert f b, L.replicate 1 $ horiz g, L.replicate 3 $ vert e c, L.replicate 1 $ horiz d]
  where
    horiz True = " ###### "
    horiz False = " ...... "

    vert b1 b2 = part b1 <> "      " <> part b2
      where
        part True = "#"
        part False = "."

encodeHexSS :: Unsigned 4 -> Vec 7 Bool
encodeHexSS n = unpack $ case n of
  0x0 -> 0b1111110
  0x1 -> 0b0110000
  0x2 -> 0b1101101
  0x3 -> 0b1111001
  0x4 -> 0b0110011
  0x5 -> 0b1011011
  0x6 -> 0b1011111
  0x7 -> 0b1110000
  0x8 -> 0b1111111
  0x9 -> 0b1111011
  0xa -> 0b1110111
  0xb -> 0b0011111
  0xc -> 0b1001110
  0xd -> 0b0111101
  0xe -> 0b1001111
  0xf -> 0b1000111

roundRobin ::
  forall n dom a.
  (KnownNat n, HiddenClockResetEnable dom) =>
  Signal dom Bool ->
  (Signal dom (Vec n Bool), Signal dom (Index n)) -- 返回 i 及其 oneHot 编码
roundRobin next = (selector, i)
  where
    i = regEn (0 :: Index n) next $ nextIdx <$> i
    selector = oneHot <$> i

muxRR ::
  (KnownNat n, HiddenClockResetEnable dom) =>
  Signal dom Bool ->
  Signal dom (Vec n a) ->
  (Signal dom (Vec n Bool), Signal dom a)
muxRR tick xs = (selector, current)
  where
    (selector, i) = roundRobin tick
    current = xs .!!. i

data SevenSegment n anodes segments dp = SevenSegment
  { anodes :: "AN" ::: Vec n (Active anodes),
    segments :: "SEG" ::: Vec 7 (Active segments),
    dp :: "DP" ::: Active dp
  }

driveSS ::
  (KnownNat n, HiddenClockResetEnable dom, _) =>
  (a -> (Vec 7 Bool, Bool)) ->
  Signal dom (Vec n (Maybe a)) ->
  Signal dom (SevenSegment n anodes segments dp)
driveSS draw digits = do
  anodes <- map toActive <$> anodes_
  segments <- map toActive <$> segments_
  dp <- toActive <$> dp_
  return SevenSegment {..}
  where
    (anodes_, digit) = muxRR (risePeriod (SNat @(Milliseconds 1))) digits -- 这一时刻, 选择 digits 中的哪一个数字, 数字的内容是什么
    (segments_, dp_) = unbundle $ maybe (repeat False, False) draw <$> digit -- 如果不显示数字, 也就是 Nothing 的情况, 那么就 repeat False(七段数码管), False(小数点)

topEntity ::
  "CLK100MHZ" ::: Clock System ->
  "SW" ::: Signal System (Vec 8 Bit) ->
  "SS" ::: Signal System (SevenSegment 4 High Low Low)
topEntity = \clk -> withClockResetEnable clk resetGen enableGen board
  where
    board switches = driveSS toSegments digits
      where
        digits = (repeat Nothing ++) . map Just . bitCoerce <$> switches
        toSegments x = (encodeHexSS x, False)
