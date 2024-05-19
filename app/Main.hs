module Main where

import Data.List (intercalate, intersperse, sort, subsequences, uncons)
import Data.List.NonEmpty (NonEmpty, fromList, head, toList)
import Data.Maybe (isJust)
import System.IO (hFlush, stdout)
import Prelude hiding (head)

main :: IO ()
main = do
  putStrLn "Welcome to Tic-Tac-Toe!"
  putStrLn "↳ Written in Haskell by Gavin Morrow"
  _ <- askMove new X
  return ()

data Player = X | O deriving (Eq, Show)

other :: Player -> Player
other X = O
other O = X

data Pos = Pos {getX :: Int, getY :: Int} deriving (Eq)

instance Show Pos where show p = "(" ++ show (getX p) ++ ", " ++ show (getY p) ++ ")"

data Size = Size {getW :: Int, getH :: Int} deriving (Eq)

area :: Size -> Int
area = (*) <$> getW <*> getH

inBounds :: Size -> Pos -> Bool
inBounds s p = getX p < getW s && getX p < getH s

data Cell = Cell {pl :: Player, pos :: Pos}

data Board = Board {cells :: [Cell], size :: Size}

new :: Board
new = Board [] $ Size 3 3

set :: Board -> Cell -> Board
set b c = b {cells = c : filter ((pos c /=) . pos) (cells b)}

get :: Board -> Pos -> Maybe Cell
get b p = fmap fst $ uncons $ filter (\c -> pos c == p) (cells b)

instance Show Board where
  show b =
    intercalate
      "\n"
      [ line
        | y <- [0 .. getH (size b) - 1],
          let line = showLine y
      ]
    where
      showLine :: Int -> String
      showLine y =
        intersperse
          ' '
          [ cell
            | x <- [0 .. getW (size b) - 1],
              let cell = case get b $ Pos x y of
                    Just (Cell X _) -> 'X'
                    Just (Cell O _) -> 'O'
                    Nothing -> '•'
          ]

askMove :: Board -> Player -> IO ()
askMove b p = do
  print b

  -- checks win on the next iteration, otherwise the board isn't updated
  if ckWin b p'
    then putStrLn $ "Player " ++ show p' ++ " won!"
    else do
      input <- prompt $ show p ++ ": "
      handleInput $ parse input
      return ()
  where
    p' = other p

    parse :: String -> Maybe Pos
    parse m = case map read $ words m of
      [] -> Nothing
      [_] -> Nothing
      [x, y] -> Just $ Pos x y
      _ -> Nothing

    occupied :: Pos -> Bool
    occupied pos' = isJust $ get b pos'

    handleInput :: Maybe Pos -> IO ()
    handleInput (Just i) = handleMove i
    handleInput Nothing = do
      putStrLn "Invalid move."
      askMove b p

    handleMove :: Pos -> IO ()
    handleMove m =
      if occupied m
        then do
          putStrLn $ "Sorry, " ++ show m ++ " is already occupied."
          askMove b p
        else do
          askMove (set b $ Cell p m) p'

ckWin :: Board -> Player -> Bool
ckWin b p
  | null cs' = False
  | otherwise = (&&) <$> ckX <*> ckY $ cs'
  where
    cs = filter ((p ==) . pl) $ cells b
    cs' = combinations 3 cs

    ckDim1 d c = allEq ls || arithmeticSeq (sort $ toList ls)
      where
        ls = fromList $ d . pos <$> c
    ckAllDim1 = all . ckDim1
    ckX = ckAllDim1 getX
    ckY = ckAllDim1 getY

ckTie :: Board -> Bool
ckTie = (==) <$> area . size <*> length . cells

allEq :: (Eq a) => NonEmpty a -> Bool
allEq l = all (== head l) l

arithmeticSeq :: [Int] -> Bool
arithmeticSeq [] = True
arithmeticSeq [_] = True
arithmeticSeq [_, _] = True
arithmeticSeq (x : y : z : xs) = y - x == z - y && arithmeticSeq (y : z : xs)

-- source: https://stackoverflow.com/a/52605612/15920018
combinations :: Int -> [a] -> [[a]]
combinations k = filter ((k ==) . length) . subsequences

-- source: https://stackoverflow.com/a/13190872/15920018
prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine
