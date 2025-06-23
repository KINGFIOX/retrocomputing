{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Chap5_3.Project where

import Chap5_2_1.Project (roundRobin)
import Clash.Prelude
import Simple

type Matrix rows cols a = Vec rows (Vec cols a)

type KeyStates rows cols = Matrix rows cols Bool

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

    -- indicesI 会根据类型生成 [0..n]
    -- map colState indicesI 相当于是
    -- for i in range(n):
    --   if i == currentCol:
    --     states[i] = rows
    states = map colState indicesI
      where
        colState thisCol = regEn (repeat False) (currentCol .== thisCol) $ map fromActive <$> rows

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
