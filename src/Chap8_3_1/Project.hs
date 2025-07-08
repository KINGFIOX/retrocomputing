{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Chap8_3_1.Project where

import Clash.Prelude
import Data.Maybe (isJust, isNothing)
import Simple

-- 小梅哥 fpga vga https://www.bilibili.com/video/BV1izCYYqEoY

-- vgaDriver :: (HiddenClockResetEnable dom, KnownNat w, KnownNat h) => VGATimings w h -> VGADriver dom w h
-- vga640x480at60 :: VGATimings 640 480
-- vga640x480at75 :: VGATimings 640 480

-- rust
-- enum VGAState<visbile, front, pulse, back> {
--   Visible(Index<visual>),
--   Visible(Index<visual>),
-- }

data VGAState visible front pulse back
  = Visible (Index visible)
  | FrontPorch (Index front)
  | SyncPulse (Index pulse)
  | BackPorch (Index back)
  deriving (Show, Generic, NFDataX)

data VGATiming (visible :: Nat) = forall front pulse back.
  VGATiming
  { polarity :: Polarity, -- 这里, 从 type-level 变成了 term-level
    preWidth :: SNat front,
    pulseWidth :: SNat pulse, -- SNat<pulse>
    postWidth :: SNat back
  }

deriving instance Show (VGATiming vis)

data VGATimings (ps :: Nat) (w :: Nat) (h :: Nat) = VGATimings
  { vgaHorizTiming :: VGATiming w,
    vgaVertTiming :: VGATiming h
  }
  deriving (Show)

visible :: VGAState visible front pulse back -> Maybe (Index visible)
visible (Visible coord) = Just coord
visible _ = Nothing

sync :: VGAState visible front pulse back -> Bool
sync SyncPulse {} = True
sync _ = False

end :: (KnownNat back) => VGAState visible front pulse back -> Bool
end (BackPorch cnt) | cnt == maxBound = True
end _ = False

type Step a = a -> a

-- using FuncPtr = VGAState<visible, front, pulse, back> (*)(VGAState<visible, front, pulse, back>);
-- struct VGACounter<visible> (FuncPtr)

data VGACounter visible
  = forall front pulse back.
    (KnownNat front, KnownNat pulse, KnownNat back) =>
    VGACounter (Step (VGAState visible front pulse back))

mkVGACounter ::
  SNat front ->
  SNat pulse ->
  SNat back ->
  Step (VGAState visible front pulse back) ->
  VGACounter visible
mkVGACounter SNat SNat SNat = VGACounter

vgaCounter ::
  (KnownNat visible) => VGATiming visible -> VGACounter visible
vgaCounter (VGATiming _ front@SNat pulse@SNat back@SNat) =
  mkVGACounter front pulse back $ \case
    Visible cnt -> count Visible FrontPorch cnt
    FrontPorch cnt -> count FrontPorch SyncPulse cnt
    SyncPulse cnt -> count SyncPulse BackPorch cnt
    BackPorch cnt -> count BackPorch Visible cnt
  where
    -- { x | x \in Index<n> } <=> { x | 0 <= x < n }
    -- unsigned int, gcc linux x64, 2^32 - 1
    count :: (KnownNat n, KnownNat m) => (Index n -> a) -> (Index m -> a) -> Index n -> a
    count this next = maybe (next 0) this . succIdx

data VGASync dom = VGASync
  { vgaHSync :: "HSYNC" ::: Signal dom Bit,
    vgaVSync :: "VSYNC" ::: Signal dom Bit,
    vgaDE :: "DE" ::: Signal dom Bool -- display enable
  }

data VGAOut dom r g b = VGAOut
  { vgaSync :: "SYNC" ::: VGASync dom,
    vgaR :: "RED" ::: Signal dom (Unsigned r),
    vgaG :: "GREEN" ::: Signal dom (Unsigned g),
    vgaB :: "BLUE" ::: Signal dom (Unsigned b)
  }

data VGADriver dom w h = VGADriver
  { vgaSync :: VGASync dom, -- 重名字段, 注意, DuplicateRecordFields
    vgaX :: Signal dom (Maybe (Index w)),
    vgaY :: Signal dom (Maybe (Index h))
  }

vgaDriver ::
  (HiddenClockResetEnable dom, KnownNat w, KnownNat h) =>
  (DomainPeriod dom ~ ps) =>
  VGATimings ps w h ->
  VGADriver dom w h
vgaDriver VGATimings {..} =
  case (vgaCounter vgaHorizTiming, vgaCounter vgaVertTiming) of
    (VGACounter nextH, VGACounter nextV) -> VGADriver {..}
      where
        stateH = register (Visible 0) $ nextH <$> stateH
        stateV = regEn (Visible 0) endLine $ nextV <$> stateV
        endLine = end <$> stateH
        vgaX = visible <$> stateH
        vgaY = visible <$> stateV
        vgaSync =
          VGASync
            { vgaDE = isJust <$> vgaX .&&. isJust <$> vgaY,
              vgaVSync = toActiveDyn (polarity vgaVertTiming) . sync <$> stateV,
              vgaHSync = toActiveDyn (polarity vgaHorizTiming) . sync <$> stateH
            }

vgaOut ::
  (HiddenClockResetEnable dom, KnownNat r, KnownNat g, KnownNat b) =>
  VGASync dom ->
  Signal dom (Unsigned r, Unsigned g, Unsigned b) ->
  VGAOut dom r g b
vgaOut vgaSync@VGASync {..} rgb = VGAOut {..}
  where
    (vgaR, vgaG, vgaB) = unbundle $ blank rgb
    blank = mux (not <$> vgaDE) $ pure (0, 0, 0)

createDomain vSystem {vName = "Dom25", vPeriod = hzToPeriod 25_175_000}

rgbwBars ::
  (KnownNat w, KnownNat h, KnownNat r, KnownNat g, KnownNat b) =>
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Index w)) ->
  Signal dom (Maybe (Index h)) ->
  Signal dom (Unsigned r, Unsigned g, Unsigned b)
rgbwBars x _ = colors !!. counter
  where
    counter = register (0 :: Index 3) $ mux (isNothing <$> x) (pure 0) $ nextIdx <$> counter
    colors = red :> green :> blue :> Nil
      where
        red = (maxBound, 0, 0)
        green = (0, maxBound, 0)
        blue = (0, 0, maxBound)

topEntity ::
  "CLK_25MHZ" ::: Clock Dom25 ->
  "RESET" ::: Reset Dom25 ->
  "VGA" ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    -- There is no type annotation in the book. However, without this, the code would analyzed with error 🐛
    board :: (HiddenClockResetEnable Dom25) => VGAOut Dom25 8 8 8
    board = vgaOut vgaSync rgb
      where
        VGADriver {..} = vgaDriver vga640x480at60
        rgb = grayAnim vgaX vgaY

vga640x480at60 :: VGATimings (HzToPeriod 25_175_000) 640 480
vga640x480at60 =
  VGATimings
    { vgaHorizTiming = VGATiming Low (SNat @16) (SNat @96) (SNat @48),
      vgaVertTiming = VGATiming Low (SNat @11) (SNat @2) (SNat @31)
    }

-- 这是一个灰度动画, 从暗到亮
grayAnim ::
  (KnownNat w, KnownNat h, KnownNat c) =>
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Index w)) ->
  Signal dom (Maybe (Index h)) ->
  Signal dom (Unsigned c, Unsigned c, Unsigned c)
grayAnim _ y = bundle (brightness, brightness, brightness)
  where
    brightness = regEn 0 endFrame $ nextIdx <$> brightness
    endFrame = isFalling False (isJust <$> y) -- posedge, 当 isJust <$> y 从高电平到低电平, 那么就输出 False
