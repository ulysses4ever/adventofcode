{-# language BangPatterns #-}
{-# language LambdaCase   #-}
module Y2021.Day16 (solve) where

import Aux ()
import Data.List as L ( concatMap, head )
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)))
import Data.Functor
import Debug.Trace
import Numeric (readHex)
import GHC.Show (intToDigit)
import Numeric.Extra (showIntAtBase)

import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Except (Except)

import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Applicative (Alternative(many))
import System.Directory (setPermissions)


-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

data B = I | O deriving (Eq, Show)
type Rec = B
type St a = StateT [B] (Except String) a

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

compute :: Int -> [Rec] -> Int
compute 1 rs = res
  where
  res = 0

compute 2 rs = res
  where
  res = 0

getPacket :: St P
getPacket = P <$> getVer <*> getPload

getPload :: St L
getPload = getTId >>=
  \case
    4   -> Lit `fmap` getLiteral
    tid -> Op tid `fmap` many getPacket

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
getNBits n = state (splitAt n)

parse :: String -> [Rec]
parse = L.concatMap readRec

readRec :: Char -> [Rec]
readRec c = bs
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

{-
Example GHCi session

λ> import  Control.Monad.Trans.State.Strict
λ> runStateT getPacket (parse "D2FE28")
ExceptT (Identity (Right (P {ver = 6, pload = Lit 2021},[O,O,O])))

-}
