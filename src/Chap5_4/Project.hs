{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Chap5_4.Project where

import Clash.Prelude
import qualified Data.List as L
import Simple
import Control.Monad (mplus)

type Matrix rows cols a = Vec rows (Vec cols a)

type KeyStates rows cols = Matrix rows cols Bool

data KeyEvent = Pressed | Released
  deriving (Show, Eq, Generic, NFDataX)

type KeyEvents rows cols = Matrix rows cols (Maybe KeyEvent)

keypadEvents ::
  (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom) =>
  Signal dom (KeyStates rows cols) ->
  Signal dom (KeyEvents rows cols)
keypadEvents current = zipWith (zipWith event) <$> delayed_ <*> current
  where
    delayed_ = register (repeat $ repeat False) current -- 延迟一个周期
    event False True = Just Pressed
    event True False = Just Released
    event _ _ = Nothing

scanKeypad ::
  (KnownNat rows, KnownNat cols, IsActive rowAct, IsActive colAct) =>
  (HiddenClockResetEnable dom) =>
  Signal dom (Vec rows (Active rowAct)) ->
  (Signal dom (Vec cols (Active colAct)), Signal dom (KeyStates rows cols))
scanKeypad rows = (map toActive <$> cols, transpose <$> bundle states)
  where
    (cols, currentCol) = roundRobin nextCol
    nextCol = riseEvery (SNat @1000)
    states = map colState indicesI
    colState thisCol = regEn (repeat False) (stable .&&. currentCol .== thisCol) (map fromActive <$> rows)
      where
        stable = cnt .== maxBound -- 列切换会有毛刺, 一开始的 10s 略过
        cnt = register (0 :: Index 10) $ mux nextCol 0 (moreIdx <$> cnt)

-- ############## seven segment ##############

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

-- ############## logic ##############

type Hex = Unsigned 4

logic ::
  forall n dom.
  (KnownNat n, HiddenClockResetEnable dom, _) =>
  Signal dom (Maybe Hex) ->
  Signal dom (Vec n (Maybe Hex))
logic key = digits
  where
    digits = regMaybe (repeat Nothing) $ update <$> key <*> digits
    update key_ digits_ = do
      newDigit <- key_
      return $ digits_ <<+ Just newDigit

-- ############## output ##############

display ::
  (KnownNat n, HiddenClockResetEnable dom, _) =>
  Signal dom (Vec n (Maybe Hex)) ->
  Signal dom (SevenSegment n _ _ _)
display = driveSS (\x -> (encodeHexSS x, False))

-- ############## input ##############

pressedKeys ::
  Matrix rows cols a ->
  KeyEvents rows cols ->
  Matrix rows cols (Maybe a)
pressedKeys = zipWith (zipWith decode)
  where
    decode mapping (Just Pressed) = Just mapping
    decode _ _ = Nothing

keymap :: Matrix 4 4 Hex
keymap =
  (1 :> 2 :> 3 :> 0xa :> Nil)
    :> (4 :> 5 :> 6 :> 0xb :> Nil)
    :> (7 :> 8 :> 9 :> 0xc :> Nil)
    :> (0 :> 0xf :> 0xe :> 0xd :> Nil)
    :> Nil

firstJust2D
  :: (KnownNat rows, KnownNat cols)
  => Matrix (rows + 1) (cols + 1) (Maybe a) -- 这个 +1 是给编译器看的, 因为 rows/cols 可能为 0, fold 要求至少得有元素
  -> Maybe a
firstJust2D = fold mplus . map (fold mplus) -- mplus 是 <|> 就是 alternative, 用于获取第一个 Just

input
  :: (HiddenClockResetEnable dom, _)
  => Signal dom (Vec 4 (Active row))
  -> (Signal dom (Vec 4 (Active col)), Signal dom (Maybe Hex))
input = inputKeypad keymap

inputKeypad
  :: (KnownNat rows, KnownNat cols, IsActive rowAct, IsActive colAct)
  => (HiddenClockResetEnable dom)
  => (KnownNat (ClockDivider dom (Milliseconds 5)))
  => Matrix (rows + 1) (cols + 1) a
  -> Signal dom (Vec (rows + 1) (Active rowAct))
  -> (
    Signal dom (Vec (cols + 1) (Active colAct)),
    Signal dom (Maybe a)
  )
inputKeypad keymap_ rows = (cols, pressedKey)
  where
    (cols, keyState) = scanKeypad rows
    events = keypadEvents $ debounce (SNat @(Milliseconds 5)) (repeat (repeat False)) keyState
    pressedKey = firstJust2D . pressedKeys keymap_ <$> events

topEntity ::
  "CLK" ::: Clock System ->
  "ROWS" ::: Signal System (Vec 4 (Active Low)) ->
  ( "SS" ::: Signal System (SevenSegment 4 High Low Low),
    "COLS" ::: Signal System (Vec 4 (Active Low))
  )
topEntity = withResetEnableGen board
  where
    board rows = (display digits, cols)
      where
        (cols, key) = input rows
        digits = logic key
