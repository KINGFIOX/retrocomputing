{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Functor law" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

{-# HLINT ignore "Redundant $" #-}

-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Chap8_5_1.Project where

import Clash.Prelude
import Control.Lens (makeLenses, zoom)
import Control.Monad.State.Strict (execState, modify)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word8)
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

-- | @maskSides@
-- 对 VGA 之类的 "先跑坐标在画像素" 的电路来说, mask 很有用
-- 把整行里前 k 个像素和后 m 个像素都屏蔽掉, 只留下中间的 n 个像素
-- @k@ 屏蔽掉前面 k 个像素
-- @raw@ 一条信号线
--   - Nothing 当前在不可见区
--   - Just i 扫描到的位置 [0 .. k + n + m - 1]
-- @return@ 一条信号线
--   - Nothing 不在 "感兴趣的中间段"
--   - Just i 在 "感兴趣的中间段", [0 .. n - 1]
maskSides ::
  (KnownNat n, KnownNat m, KnownNat k) =>
  (HiddenClockResetEnable dom) =>
  SNat k ->
  Signal dom (Maybe (Index (k + n + m))) ->
  Signal dom (Maybe (Index n))
maskSides k raw = transformed
  where
    changed_ = register Nothing raw ./=. raw
    started = raw .== Just (snatToNum k) -- 说明扫描枪已经扫描到第 k 个像素了, 开始技术
    r = register Nothing transformed -- 保存结果
    transformed =
      mux (not <$> changed_) r $ -- 如果坐标不变, 沿用上一拍的结果
        mux (isNothing <$> raw) (pure Nothing) $ -- 否则, 如果 raw 是 Nothing, 说明扫描枪已经扫描到不可见区了, 返回 Nothing
          mux started (pure $ Just 0) $ -- 如果扫描到了第 k 个像素, 开始计数
            (succIdx =<<) <$> r

maskStart ::
  forall k n dom.
  (KnownNat k, KnownNat n) =>
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Index (k + n))) ->
  Signal dom (Maybe (Index n))
maskStart = maskSides (SNat @k) -- 屏蔽前 k 个像素

maskEnd ::
  forall k n dom.
  (KnownNat n, KnownNat k) =>
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Index (n + k))) ->
  Signal dom (Maybe (Index n))
maskEnd = maskSides (SNat @0) -- 前面不屏蔽, 总长是 n+k, 感兴趣的段是 n 个像素, 后面 k 个像素就不再显示了, 其实也就是被屏蔽了

center ::
  forall n n0 k m dom.
  (KnownNat n, KnownNat n0, KnownNat k, KnownNat m) =>
  (k ~ ((n0 - n) `Div` 2), n0 ~ (k + n + m)) => -- k = \floor ((n0 - n) / 2), 原来的长度是 n0. 居中, k: 左边要屏蔽掉多少
  (HiddenClockResetEnable dom) =>
  Signal dom (Maybe (Index n0)) ->
  Signal dom (Maybe (Index n))
center = maskSides (SNat @k)

-- | @scale@
-- 放缩信号, 把 n*k 个像素放缩为 n 个像素
-- @k@ 放缩因子, k 个像素为一组, 然后一组里面取一个代表像素
-- @raw@ 原始信号为 n*k 个像素, 放缩后为 n 个像素
-- @return@ 组编号、组内的像素编号
scale ::
  forall n k dom.
  (KnownNat n, KnownNat k, 1 <= k) =>
  (HiddenClockResetEnable dom) =>
  SNat k -> -- 放缩因子, k 个像素为一组, 然后一组里面取一个代表像素
  Signal dom (Maybe (Index ((*) n k))) -> -- 原始信号为 n*k 个像素, 放缩后为 n 个像素
  (Signal dom (Maybe (Index n)), Signal dom (Maybe (Index k)))
scale _ raw = (scaledNext, enable (isJust <$> scaledNext) counterNext)
  where
    prev = register Nothing raw -- 上一拍 (像素时钟)
    changed_ = raw ./=. prev -- 是否变化
    counter = register 0 counterNext -- 计数器
    counterNext =
      mux (not <$> changed_) counter $ -- 如果没有发生变化, 计数器不变
        mux (isNothing <$> prev) (pure 0) $ -- 如果上一拍是 Nothing, 说明扫描枪已经扫描到不可见区了, 计数器清零
          nextIdx <$> counter -- 否则, 计数器自增
    scaled = register Nothing scaledNext -- 组编号
    scaledNext =
      mux (not <$> changed_) scaled $ -- 如果没有发生变化, 放缩后的信号不变
        mux (counterNext .== 0) (maybe (Just 0) succIdx <$> scaled) $ -- 只有计数器回到 0 时 scaled++
          scaled

type Coord = Signed 10

data St = MkSt
  { _ballH, _ballV :: (Coord, Coord)
  }
  deriving (Show, Generic, NFDataX)

makeLenses ''St

type ScreenWidth = 640

type ScreenHeight = 480

screenWidth :: Coord
screenWidth = snatToNum (SNat @ScreenWidth)

screenHeight :: Coord
screenHeight = snatToNum (SNat @ScreenHeight)

initState :: St
initState =
  MkSt
    { _ballH = (10, 2), -- 水平坐标，水平速度
      _ballV = (100, 3)
    }

newtype Params = MkParams
  { ballSize :: Coord
  }
  deriving (Show, Generic, NFDataX)

defatultParams :: Params
defatultParams = MkParams {ballSize = 35}

-- execState :: MonadState s m => s -> m a -> m s
-- 第一个参数是 MonadState
-- 第二个参数是一个状态转移函数

-- zoom <field> <action>, 就是: 对某个类型的 <field> 字段，执行 <action> 操作
-- 比如: zoom ballV $ modify $ bounceBetween (0, screenHeight - ballSize)
-- 就是: 对 ballV 字段，执行 modify (bounceBetween (0, screenHeight - ballSize)) 操作
-- 然后 modify 接受一个函数, 这个函数接受一个状态, 然后返回一个状态

-- | @updateState@
-- 输入一组参数，输入旧的状态，输出新的状态
updateState :: Params -> St -> St
updateState _params@MkParams {..} = execState $ do
  zoom ballV $ modify $ bounceBetween (0, screenHeight - ballSize) -- zoom ballV 表示: 只修改 ballV 字段
  zoom ballH $ modify $ bounceBetween (0, screenWidth - ballSize) -- modify (bounceBetween (0, screenWidth - ballSize)), modify 接受一个函数，然后修改状态

type Color = (Word8, Word8, Word8)

draw :: Params -> St -> Index ScreenWidth -> Index ScreenHeight -> Color
draw MkParams {..} MkSt {..} ix iy
  | isBall = yellow
  | otherwise = gray
  where
    x = fromIntegral ix
    y = fromIntegral iy

    between z (lo, hi) = lo <= z && z <= hi
    -- 判断 (x, y) 是否在矩阵 (x0, y0) 到 (x0 + w, y0 + h) 内, 扫描枪扫描到的点, 是否留在了小球的范围内
    rect (x0, y0) (w, h) = x `between` (x0, x0 + w) && y `between` (y0, y0 + h)

    (ballX, _) = _ballH
    (ballY, _) = _ballV
    isBall = rect (ballX, ballY) (ballSize, ballSize)

    yellow = (0xff, 0xff, 0x00)
    gray = (0x80, 0x80, 0x80)

createDomain vSystem {vName = "Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity ::
  "CLK_25MHZ" ::: Clock Dom25 ->
  "RESET" ::: Reset Dom25 ->
  "VGA" ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen $ board
  where
    board :: (HiddenClockResetEnable Dom25) => VGAOut Dom25 8 8 8
    board = vgaOut vgaSync rgb
      where
        VGADriver {..} = vgaDriver vga640x480at60
        frameEnd = isFalling False (isJust <$> vgaY)
        st = regEn initState frameEnd $ updateState defatultParams <$> st
        rgb = fmap (maybe (0, 0, 0) bitCoerce) $ liftA2 <$> (draw defatultParams <$> st) <*> vgaX <*> vgaY
