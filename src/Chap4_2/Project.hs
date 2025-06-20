module Chap4_2.Project where

import Clash.Explicit.Prelude

helloRegister :: Clock System -> Reset System -> Enable System -> Signal System Bool
helloRegister clk rst en = register clk rst en True (pure False)

-- >>> sampleN 5 $ helloRegister clockGen resetGen enableGen
-- [True,True,False,False,False]

-- This is. . . quite unexpected, isn’t it?
-- Why is the output True for two cycles instead of just one?
-- The reason is that the first simulated timestep is before the first clock cycle;
-- then in the first cycle, the value coming from the register into the output is True,
-- and False is written back to the register for the next cycle.
-- 这句话也就是在说: cycle0 略过(其实对应的是 X, 未定值), cycle1 才是按下 reset 后的第一个周期

flippy :: Clock System -> Reset System -> Enable System -> Signal System (Bool, Bool)
flippy clk rst en = bundle (r, r')
  where
    r = register clk rst en True r'
    r' = not <$> r

-- >>> import qualified Data.List as L
-- >>> mapM_ print $ Data.List.zip [0..] $ sampleN 8 $ flippy clockGen resetGen enableGen
-- (0,(True,False))
-- (1,(True,False))
-- (2,(False,True))
-- (3,(True,False))
-- (4,(False,True))
-- (5,(True,False))
-- (6,(False,True))
-- (7,(True,False))
