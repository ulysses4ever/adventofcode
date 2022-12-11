{-# language BangPatterns   #-}
{-# language BlockArguments #-}
module Y2021.Day4 (solve) where

import Aux

import Data.List
import Data.Either
import Debug.Trace
import Data.List.Extra (chunksOf)

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)

import qualified Data.Map.Strict as MM
import Data.Map.Strict (Map)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Vector (Vector)

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

matSize :: Int
matSize = 5

data St = S Ns [Mt] deriving Show
data St' = S' Ws St  deriving Show
type Ws = Vector Win
type Ns = [Int]
type Pos = (Int,Int)
type MCt = IntMap Pos
type Fl = Vector (Vector Bool)
data Mt = Mt MCt Fl

compute :: Int -> St -> Int
compute 1 s = go s
  where
  go :: St -> Int
  go s = case update s of
   Left score -> score
   Right s'   -> go s'

compute 2 s@(S _ ms) = go (S' (V.replicate (length ms) N) s)
  where
  go :: St' -> Int
  go s = case update' s of
   Left score -> score
   Right s'   -> go s'

update' :: St' -> Either Int St'
update' (S' ws (S (n:ns) ms)) = res
  where
  rs = map (updateBoard n) ms
  mm = findIndices (\(i, (w, _)) -> w == Y && ws V.! i == N) (zip [0..] rs)
  ms' = map snd rs
  chk ws i = V.modify (\v -> VM.write v i Y) ws
  res = case mm of
    [i] ->
      if V.all (== Y) ws'
      then Left $ score n (ms' !! i)
      else Right (S' ws' $ S ns ms')
      where
      ws' = chk ws i
    xs -> Right (S' (foldl' chk ws mm) $ S ns ms')

update :: St -> Either Int St
update (S (n:ns) ms) = res
  where
  rs = map (updateBoard n) ms
  res = case lookup Y rs of
    Nothing -> Right (S ns $ map snd rs)
    Just m  -> Left  $ score n m

score :: Int -> Mt -> Int
score n (Mt mct fl) = n * sum unckd
  where
  unckd = map fst $ filter (\(x, (r,c)) -> not (fl V.! r V.! c)) $ M.toList mct

data Win = Y | N
  deriving (Eq, Show)

updateBoard :: Int -> Mt -> (Win, Mt)
updateBoard n m@(Mt mct fl) = case M.lookup n mct of
  Nothing    -> (N, m)
  Just p     -> case updateFillings fl p of
    (w, fl') -> (w, Mt mct fl')

updateFillings :: Fl -> Pos -> (Win, Fl)
updateFillings fl (r, c) = (w, fl')
  where
  fl' = V.modify (\fl -> do
          VM.modify fl (chk c) r
          VM.modify fl (chk r) (matSize + c)
          ) fl
  chk i = V.modify (\v -> VM.write v i True)
  w = if V.all (== True) (fl' V.! r) ||
    V.all (== True) (fl' V.! (matSize + c))
    then Y
    else N

parse :: String -> St
parse i = S ns ms
  where
  (ns' : emp : rest) = lines i
  ns = readIntsSep ',' ns'
  ms' = split null rest
  ms = map readMatrix ms'

emptyFl = res
  where
  emp = V.replicate matSize False
  res = V.replicate (2 * matSize) emp

readMatrix :: [String] -> Mt
readMatrix xss = Mt mct emptyFl
  where
  mct :: MCt
  mct = M.fromList do
    (r, rw) <- zip [0..] xss
    (c, x)  <- zip [0..] (words rw)
    pure (read x, (r,c))

-- Debug

showMt (Mt mct fl) = res
  where
  pos = map prn $ sortOn snd $ M.toList mct
  prn (x, (r,c)) = (if fl V.! r V.! c then "x_" else "__") ++ show x
  res = '\n' : concatMap (\rw -> show rw ++ "\n") (chunksOf matSize pos)

instance Show Mt where
  show = showMt
