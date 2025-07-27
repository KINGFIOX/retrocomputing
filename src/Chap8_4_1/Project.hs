{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

{-# HLINT ignore "Functor law" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Chap8_4_1.Project where

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
        rgb = bouncingBall vgaX vgaY

vga640x480at60 :: VGATimings (HzToPeriod 25_175_000) 640 480
vga640x480at60 =
  VGATimings
    { vgaHorizTiming = VGATiming Low (SNat @16) (SNat @96) (SNat @48),
      vgaVertTiming = VGATiming Low (SNat @11) (SNat @2) (SNat @31)
    }

type BallSize = 35

-- 这是一个弹跳小球的动画生成器
bouncingBall ::
  (KnownNat w, KnownNat h, KnownNat r, KnownNat g, KnownNat b) =>
  ((BallSize + 2) <= w, (BallSize + 1) <= h) => -- 约束 w 和 h 至少是 BallSize + 2, BallSize + 1
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Index w)) ->
  Signal dom (Maybe (Index h)) ->
  Signal dom (Unsigned r, Unsigned g, Unsigned b)
bouncingBall vgaX vgaY = draw
  where
    draw = mux isBall ballColor backColor -- (vgaX, vgaY) 是否在小球的范围
    isBall = (near <$> ballX <*> vgaX) .&&. (near <$> ballY <*> vgaY)
      where
        near x0 = maybe False $ \(fromIntegral -> x) -> x0 <= x && x < (x0 + ballSize)

    ballColor = pure (0xf0, 0xe0, 0x40) -- Yellow
    backColor = pure (0x30, 0x30, 0x30) -- Dark gray
    ballSize :: (Num a) => a
    ballSize = snatToNum (SNat @BallSize)

    rightWall = maxOf vgaX - ballSize
    bottomWall = maxOf vgaY - ballSize

    maxOf :: forall n p. (KnownNat n, 1 <= n) => p (Maybe (Index n)) -> Signed (CLog 2 n + 1)
    maxOf _ = fromIntegral (maxBound :: Index n)

    frameEnd = isFalling False (isJust <$> vgaY)

    (ballX, speedX) = unbundle $ regEn (0, 2) frameEnd $ bounceBetween (0, rightWall) <$> bundle (ballX, speedX)
    (ballY, speedY) = unbundle $ regEn (0, 1) frameEnd $ bounceBetween (0, bottomWall) <$> bundle (ballY, speedY)

move :: (Num a) => (a, a) -> (a, a)
move (x, dx) = (x + dx, dx)

reflect :: (Num a, Num a', Ord a, Ord a') => (a, a') -> (a, a') -> (a, a')
reflect (p, n) (x, dx)
  | sameDirection n dist = (p + dist, negate dx)
  | otherwise = (x, dx)
  where
    -- 如果碰到了上边界, p - x 就是 -, 然后 -1 也是 -, 同号
    -- 如果没碰到上边界, p - x 就是 +, 然后 -1 是 -, 异号
    sameDirection u v = compare 0 u == compare 0 v
    dist = p - x -- 边界与小球的距离

-- 第一个参数: 边界
bounceBetween :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a)
bounceBetween (lo, hi) = reflect (lo, 1) . reflect (hi, -1) . move -- 碰到了上界, 往回走 -1; 碰到了下界, 往正方向走

scalableRGBBars1 x _ = colors !!. counter
  where
    newColumn = changed Nothing x -- x0, 初值为 Nothing
    counter = regEn (0 :: Index 3) newColumn $ mux (isNothing <$> x) (pure 0) (nextIdx <$> counter) -- 彩条
    colors = red :> green :> blue :> Nil
      where
        red = (maxBound, 0, 0)
        green = (0, maxBound, 0)
        blue = (0, 0, maxBound)

scalableRGBBars x _ = colors !!. counterNext -- 前递了, 上面那种是延迟了一个周期
  where
    newColumn = changed Nothing x
    counter = register (0 :: Index 3) counterNext
    counterNext =
      mux (not <$> newColumn) counter $
        mux (isNothing <$> x) (pure maxBound) $
          nextIdx <$> counter
    colors = red :> green :> blue :> Nil
      where
        red = (maxBound, 0, 0)
        green = (0, maxBound, 0)
        blue = (0, 0, maxBound)
