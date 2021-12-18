{-# language FlexibleContexts #-}
{-# language LambdaCase       #-}
module Y2021.Day16 where

import Data.List as L ( concatMap, head )
import Data.Functor
import Data.Bool
import Debug.Trace
import Numeric (readHex)
import GHC.Show (intToDigit)
import Numeric.Extra (showIntAtBase)

import Control.Arrow
import Control.Monad (guard, replicateM, when)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import Control.Monad.Trans.Except (Except, runExcept)

import Control.Monad.State.Class
import Control.Applicative (Alternative(many))


-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

data B = I | O deriving (Eq, Show)
type Rec = B
type T = ([B], Int) -- state
type St a = StateT T (Except String) a -- the monad

type Ver = Int
type TId = Int
data P = P
  { ver   :: Ver
  , pload :: L }
  deriving Show
data L -- payload
  = Lit Integer
  | Op TId [P]
  deriving Show

compute :: Int -> [Rec] -> Integer
compute n bs = -- traceShow p
  res n
  where
    (p, _bs_and_cnt) = run getPacket bs
    res 1 = fromIntegral $ sumVer p
    res 2 = eval p

eval :: P -> Integer
eval = \case
  P _ (Lit n)    -> n
  P _ (Op op ps) -> case op of
    0 -> sum es
    1 -> product es
    2 -> minimum es
    3 -> maximum es
    5 -> bool 0 1 (e1 > e2)
    6 -> bool 0 1 (e1 < e2)
    7 -> bool 0 1 (e1 == e2)
    where
      es = map eval ps
      [e1, e2] = es

sumVer :: P -> Int
sumVer (P v (Lit _)) = v
sumVer (P v (Op _ ps)) = v + (sum $ map sumVer ps)

run :: St a -> [B] -> (a, ([B], Int))
run a bs =
  either (\s -> error $ "run: " ++ s) id .
  runExcept $ runStateT a (bs, 0)

getPacket :: St P
getPacket = P <$> getVer <*> getPload

getPload :: St L
getPload = getTId >>= \case
    4   -> Lit `fmap` getLiteral
    tid -> Op tid `fmap` getSubpackets

getSubpackets :: St [P]
getSubpackets = getBit >>= \case
    O -> getNBitInt 15 >>= getNBits >>= subpackets
    I -> getNBitInt 11 >>= flip replicateM getPacket

  where
    subpackets bs = do
      let (ps, (bs', cnt')) = run (many getPacket) bs
      guard $ null bs'
      modify (\(bs, cnt) -> (bs, cnt+cnt'))
      pure ps

getLiteral :: St Integer
getLiteral = bitsToInt <$> go []
  where
    go bs = do
      f <- getBit
      bs' <- getNBits 4
      let res = bs ++ bs'
      case f of
        I -> go res
        O -> pure res

getTId :: St Int
getTId = getNBitInt 3

getVer :: St Ver
getVer = getNBitInt 3

getNBitInt :: Integral i => Int -> St i
getNBitInt = fmap bitsToInt . getNBits

getBit :: St B
getBit = getNBits 1 <&> head

getNBits :: Int -> St [B]
getNBits n = do
  (bs, cnt) <- get
  guard $ length bs >= n
  let (res, bs') = splitAt n bs
  put (bs', cnt + n)
  pure res

getCnt :: St Int
getCnt = snd <$> get

parse :: String -> [Rec]
parse = L.concatMap readRec . filter (/= '\n')

readRec :: Char -> [Rec]
readRec c =
  bs
  where
    bss = showBin . fst . L.head . readHex . pure $ c
    bs  = map readBit $
          replicate (4 - length bss) '0' ++ bss

readBit :: Char -> B
readBit c = case c of
  '0' -> O
  '1' -> I
  _   -> error "readBit partial"

bitToInt :: Integral i => B -> i
bitToInt = \case
  O -> 0
  I -> 1

bitsToInt :: Integral i => [B] -> i
bitsToInt = foldr (\b r -> bitToInt b + r * 2) 0 . reverse

-- in base since GHC 9.2 but relude fails to compile with it
showBin :: (Integral a, Show a) => a -> String
showBin n = showIntAtBase 2 intToDigit n ""
