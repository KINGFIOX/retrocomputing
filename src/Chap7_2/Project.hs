{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Chap7_2.Project where

import Clash.Prelude
import Simple

-- 小梅哥 fpga vga https://www.bilibili.com/video/BV1izCYYqEoY

data VGADriver dom w h = VGADriver
  { vgaHSync :: Signal dom Bit,
    vgaVSync :: Signal dom Bit,
    vgaX :: Signal dom (Maybe (Index w)),
    vgaY :: Signal dom (Maybe (Index h))
  }

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

vga640x480at60 :: VGATimings (HzToPeriod 25_175_000) 640 480
vga640x480at60 =
  VGATimings
    { vgaHorizTiming = VGATiming Low (SNat @16) (SNat @96) (SNat @48),
      vgaVertTiming = VGATiming Low (SNat @11) (SNat @2) (SNat @31)
    }

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
        vgaHSync = toActiveDyn (polarity vgaHorizTiming) . sync <$> stateH
        vgaY = visible <$> stateV
        vgaVSync = toActiveDyn (polarity vgaVertTiming) . sync <$> stateV
