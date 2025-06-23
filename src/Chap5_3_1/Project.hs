{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Chap5_3_1.Project where

import Clash.Prelude
import Simple

type Matrix rows cols a = Vec rows (Vec cols a)

type KeyStates rows cols = Matrix rows cols Bool

data KeyEvent = Pressed | Released
  deriving (Show, Eq, Generic, NFDataX)

type KeyEvents rows cols = Matrix rows cols (Maybe KeyEvent)

keypadEvents ::
  (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom) =>
  Signal dom (KeyStates rows cols) ->
  Signal dom (KeyEvents rows cols)
-- zipWith_in :: (Bool -> Bool -> Maybe KeyEvent) -> Vec cols Bool -> Vec cols Bool -> Vec cols (Maybe KeyEvent)
-- zipWith_in event :: Vec cols Bool -> Vec cols Bool -> Vec cols (Maybe KeyEvent)
-- zipWith_out :: (Vec cols Bool -> Vec cols Bool -> Vec cols (Maybe KeyEvent)) -> Vec rows (Vec cols Bool) -> Vec rows (Vec cols Bool) -> Vec rows (Vec cols (Maybe KeyEvent))
-- zipWith_out (zipWith_in event) :: Vec rows (Vec cols Bool) -> Vec rows (Vec cols Bool) -> Vec rows (Vec cols (Maybe KeyEvent))
-- zipWith (zipWith event) <$> :: Signal dom (Vec rows (Vec cols Bool)) -> Signal dom (Vec rows (Vec cols Bool) -> KeyEvents rows cols)
-- zipWith (zipWith event) <$> delayed :: Signal dom (Vec rows (Vec cols Bool) -> KeyEvents rows cols)
-- zipWith (zipWith event) <$> delayed <*> :: Signal dom (Vec rows (Vec cols Bool)) -> Signal dom (KeyEvents rows cols)
-- zipWith (zipWith event) <$> delayed <*> current :: Signal dom (KeyEvents rows cols)
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
    (cols, currentCol) = roundRobin nextCol -- 每脉冲一次, 扫描列移动
      where
        nextCol = riseEvery (SNat @1000) -- 脉冲
    states = map colState indicesI
      where
        colState thisCol = regEn (repeat False) (currentCol .== thisCol) $ map fromActive <$> rows

-- toggle 切换
toggleKeypad ::
  (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom) =>
  Signal dom (KeyEvents rows cols) ->
  Signal dom (KeyStates rows cols)
toggleKeypad events = toggles
  where
    clicks = map (map (== Just Pressed)) <$> events
    toggles = bundle . map (bundle . map toggleState . unbundle) . unbundle $ clicks
      where
        toggleState click = let r = regEn False click (not <$> r) in r

topEntity ::
  Clock System ->
  Signal System (Vec 4 (Active Low)) ->
  ( Signal System (Vec 16 (Active Low)),
    Signal System (Vec 4 (Active Low))
  )
topEntity = withResetEnableGen board
  where
    board rows = (map toActive <$> leds, cols)
      where
        (cols, keyStates) = scanKeypad rows
        leds = bitCoerce <$> keyStates -- 将矩阵转换成一排 Bool
