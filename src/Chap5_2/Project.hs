{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Chap5_2.Project where

import Clash.Prelude
import Simple

topEntity ::
  Clock System ->
  Signal System (Vec 8 Bit) ->
  (Signal System (Vec 4 (Active High)), Signal System (Vec 7 (Active Low)), Signal System (Active Low))
topEntity = withResetEnableGen board
  where
    board switches =
      ( map toActive <$> anodes,
        map toActive <$> segments,
        toActive <$> dp -- decimal point(小数点)
      )
      where
        segments = pure $ repeat True -- 这里是因为还没有接入sevenSegementEncoder，所以先用全1（也就是显示8来代替）
        dp = pure False
        fast = riseRate (SNat @512)
        slow = fast .&&. cnt .==. 0
          where
            speed = bitCoerce <$> switches
            cnt = regEn (0 :: Unsigned 8) fast $ mux (cnt .>=. speed) 0 (cnt + 1)
        i = regEn 0 slow (nextIdx <$> i)
        anodes = oneHot <$> i
