{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Chap3_4.Project where

import Clash.Prelude
import qualified Data.List as L

data Polarity = High | Low

newtype Active (p :: Polarity) = MkActive {activeLevel :: Bit}

active :: Bit -> Active p
active = MkActive

class IsActive p where
  fromActive :: Active p -> Bool
  toActive :: Bool -> Active p

instance IsActive High where
  fromActive = bitToBool . activeLevel
  toActive = MkActive . boolToBit

instance IsActive Low where
  fromActive = bitToBool . complement . activeLevel
  toActive = MkActive . complement . boolToBit

ss5 :: Vec 7 Bool
ss5 = True :> False :> True :> True :> False :> True :> True :> Nil

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

topEntity ::
  "SWITCHES" ::: Signal System (Vec 4 Bit) ->
  "SS"
    ::: ( "AN" ::: Signal System (Vec 4 (Active High)),
          "SEG" ::: Signal System (Vec 7 (Active Low)),
          "DP" ::: Signal System (Active Low)
        )
topEntity sws = (map toActive <$> anodes, map toActive <$> segments, toActive <$> dp)
  where
    anodes = pure $ False :> False :> False :> True :> Nil
    digit = bitCoerce <$> sws
    segments = encodeHexSS <$> digit
    dp = pure False
