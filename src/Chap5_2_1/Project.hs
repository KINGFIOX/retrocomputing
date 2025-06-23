{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Chap5_2_1.Project where

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

topEntity ::
  Clock System ->
  Signal System (Vec 8 Bit) ->
  (Signal System (Vec 4 (Active High)), Signal System (Vec 7 (Active Low)), Signal System (Active Low))
topEntity = withResetEnableGen board
  where
    board switches = (map toActive <$> anodes, map toActive <$> segments, toActive <$> dp)
      where
        digits = (repeat Nothing ++) <$> (map Just . bitCoerce <$> switches)
        (anodes, digit) = muxRR (riseRate (SNat @512)) digits
        segments = toSegments <$> digit
          where
            toSegments = maybe (repeat False) encodeHexSS
        dp = pure False
