{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Chap5_3_2.Project where

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
        -- 对整个键盘进行消抖. 但我其实觉得有点问题, 就是键盘的状态很难变为全 0, 所以我觉得这个消抖其实有瑕疵
        keyStates' = debounce (SNat @(Milliseconds 5)) (repeat $ repeat False) keyStates
        ledStates = toggleKeypad . keypadEvents $ keyStates'
        leds = bitCoerce <$> ledStates
