{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Chap6_4.Project where

import Chap5_4.Project (SevenSegment, driveSS, encodeHexSS, inputKeypad, keymap)
import Clash.Prelude
import Data.Char (intToDigit)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Simple

data Op = Add | Subtract
  deriving (Show, Generic, NFDataX)

data St n = MkSt
  { value :: BCD n,
    opBuf :: Op,
    inputBuf :: Maybe (BCD n) -- Nothing: 显示上一次计算的结果
  }
  deriving (Show, Generic, NFDataX)

initSt :: (KnownNat n) => St n
initSt = MkSt {value = repeat 0, opBuf = Add, inputBuf = Nothing}

removeLeadingZeroes :: (KnownNat n) => Vec n Digit -> Vec n (Maybe Digit)
removeLeadingZeroes digits = case mapAccumL step False digits of
  (False, _) -> repeat Nothing <<+ Just 0
  (True, digits') -> digits'
  where
    step False 0 = (False, Nothing) -- tuple 第一个分量: 有效位是否出现
    step _ d = (True, Just d)

displayedDigits :: (KnownNat n) => St n -> Vec n (Maybe Digit)
displayedDigits MkSt {..} = removeLeadingZeroes $ fromMaybe value inputBuf

-- >>> removeLeadingZeroes (0 :> 0 :> 1 :> 2 :> Nil)
-- Nothing :> Nothing :> Just 1 :> Just 2 :> Nil

prettyDigits :: Vec n (Maybe Digit) -> String
prettyDigits = L.map (maybe ' ' (intToDigit . fromIntegral)) . toList

-- >>> putStrLn $ prettyDigits $ removeLeadingZeroes (0 :> 0 :> 1 :> 2 :> Nil)
-- 12

type BCD n = Vec n Digit

type Digit = Index 10

fromDigit :: Digit -> Unsigned 4
fromDigit = bitCoerce

addBCD :: BCD n -> BCD n -> BCD n
addBCD xs ys = snd . mapAccumR addDigit False $ zip xs ys
  where
    addDigit :: Bool -> (Digit, Digit) -> (Bool, Digit)
    addDigit c (x, y) = (c', fromIntegral z')
      where
        z :: Unsigned 5
        z = extend (fromDigit x) + extend (fromDigit y) + if c then 1 else 0
        (c', z') = if z <= 9 then (False, z) else (True, z - 10)

subBCD :: BCD n -> BCD n -> BCD n
subBCD xs ys = snd . mapAccumR subDigit False $ zip xs ys
  where
    subDigit :: Bool -> (Digit, Digit) -> (Bool, Digit)
    subDigit b (x, y) = (b', fromIntegral z')
      where
        z = sub (fromDigit x) (fromDigit y) - if b then 1 else 0
        (b', z') = if z <= 9 then (False, z) else (True, z + 10)

data Cmd
  = Digit Digit
  | Op Op
  | Backspace
  | Clear
  | Equals
  deriving (Show, Generic, NFDataX)

update :: (KnownNat n) => Cmd -> St n -> St n
update Clear _ = initSt
update (Digit d) s@MkSt {..} = s {inputBuf = Just $ fromMaybe (repeat 0) inputBuf <<+ d}
update Backspace s@MkSt {..} = s {inputBuf = Just $ 0 +>> fromMaybe (repeat 0) inputBuf}
update Equals s@MkSt {..} = compute s
update (Op op) s = compute s {opBuf = op}

compute :: (KnownNat n) => St n -> St n
compute s@MkSt {..} = s {value = newValue, inputBuf = Nothing}
  where
    newValue = case opBuf of
      Add -> maybe value (addBCD value) inputBuf
      Subtract -> maybe value (subBCD value) inputBuf

type Hex = Unsigned 4

keyToCmd :: Hex -> Maybe Cmd
keyToCmd n | n <= 9 = Just $ Digit $ bitCoerce n
keyToCmd 0xa = Just $ Op Add
keyToCmd 0xb = Just Backspace
keyToCmd 0xc = Just Clear
keyToCmd 0xd = Just $ Op Subtract
keyToCmd 0xe = Just Equals
keyToCmd _ = Nothing

testInput :: Signal dom (Maybe Hex)
testInput = fromList . (<> L.repeat Nothing) . stretch $ [9, 3, 0xa, 1, 4, 0xd, 6, 0xe]
  where
    stretch = L.concatMap $ \x -> [Nothing, Just x, Nothing]

-- >>> mapM_ (print . toList . map (maybe ' ' (intToDigit . fromIntegral))) $  L.map L.head . L.group $ sampleN @System (13 * 3) $ logic @4 . fmap (keyToCmd =<<) $ testInput

logic ::
  forall n dom.
  (KnownNat n, HiddenClockResetEnable dom) =>
  Signal dom (Maybe Cmd) ->
  Signal dom (Vec n (Maybe Digit))
logic = moore (flip (maybe id update)) displayedDigits initSt

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
        display = driveSS (\x -> (encodeHexSS . bitCoerce $ x, False))
        input = inputKeypad keymap
        digits = logic cmd
        (cols, key) = input rows
        cmd = (keyToCmd =<<) <$> key
