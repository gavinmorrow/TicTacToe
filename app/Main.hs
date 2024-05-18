import Data.List (intercalate, intersperse, sort, subsequences, uncons)
import Data.List.NonEmpty (NonEmpty, fromList, head, toList)
import Data.Maybe (isJust)
import Prelude hiding (head)

data Player = X | O deriving (Eq, Show)

other :: Player -> Player
other X = O
other O = X

data Pos = Pos {x :: Int, y :: Int} deriving (Eq)

instance Show Pos where show p = "(" ++ show (x p) ++ ", " ++ show (y p) ++ ")"

data Size = Size {w :: Int, h :: Int} deriving (Eq)

inBounds :: Size -> Pos -> Bool
inBounds s p = x p < w s && y p < h s

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
        | y <- [0 .. h (size b) - 1],
          let line = showLine b y
      ]
    where
      showLine :: Board -> Int -> String
      showLine b y =
        intersperse
          ' '
          [ cell
            | x <- [0 .. w (size b) - 1],
              let cell = case get b $ Pos x y of
                    Just (Cell X _) -> 'X'
                    Just (Cell O _) -> 'O'
                    Nothing -> '•'
          ]

main = askMove new X

askMove :: Board -> Player -> IO Board
askMove b p = do
  print b

  -- checks win on the next iteration, otherwise the board isn't updated
  if ckWin b p'
    then do
      putStrLn $ "Player " ++ show p' ++ " won!"
      return b
    else do
      putStr $ show p ++ ": "

      input <- getLine
      handleMove $ parse input
  where
    p' = other p

    parse :: String -> Pos
    parse m = Pos x y where [x, y] = map read $ words m

    occupied :: Pos -> Bool
    occupied pos = isJust $ get b pos

    handleMove m = do
      if occupied m
        then do
          putStrLn $ "Sorry, " ++ show m ++ " is already occupied."
          askMove b p
        else do
          askMove (set b $ Cell p m) p'

ckWin :: Board -> Player -> Bool
ckWin b p
  | null cs' = False
  | otherwise = all (ckDim1 x) cs' && all (ckDim1 y) cs'
  where
    cs = filter ((p ==) . pl) $ cells b
    cs' = combinations 3 cs
    ckDim1 d c = allEq ls || arithmeticSeq (sort $ toList ls)
      where
        ls = fromList $ fmap (d . pos) c

allEq :: (Eq a) => NonEmpty a -> Bool
allEq l = all (== head l) l

arithmeticSeq :: [Int] -> Bool
arithmeticSeq [] = True
arithmeticSeq [_] = True
arithmeticSeq [_, _] = True
arithmeticSeq (x : y : z : xs) = (y - x) == (z - y) && arithmeticSeq (y : z : xs)

-- source: https://stackoverflow.com/a/52605612/15920018
combinations :: Int -> [a] -> [[a]]
combinations k = filter ((k ==) . length) . subsequences