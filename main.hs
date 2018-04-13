module Chess where

import System.IO
import Control.Concurrent
import System.Console.ANSI
data Colour = W | B | None deriving (Show, Read, Eq, Ord)
data Piece = Empty | Pawn | Queen deriving (Show, Read, Eq, Ord)
type Square = (Piece, Colour)
type Board = [[Square]]

makeSquare (t, c) = (t, c)

getRow (t, c) = map (\x -> (t, c)) [1..8]

makeBoard (t, c) = map (\x -> getRow (t, c)) [1..8]

printsquare (t, c)
  | c == None = putStr " _ "
  | t == Pawn && c == W = putStr " P "
  | t == Pawn && c == B = putStr " p "
  | t == Queen && c == W = putStr " Q "
  | t == Queen && c == B = putStr " q "

printrow row = do
  mapM printsquare row

print_wall x = do
  putStr "    +"
  mapM (\y -> putStr x) [1..24]
  putStrLn "+"

print_x_coords = do
  putStr "      "
  mapM (\x -> do
    putStr x
    putStr "  "
    ) ["a", "b", "c", "d", "e", "f", "g", "h"]
  putStr "\n"

printboard board = do
  let indexed_board = zip (reverse [1..8]) board
  print_x_coords
  print_wall "-"
  mapM (\x -> do
      putStr (show (fst x))
      putStr "   |"
      printrow (snd x)
      putStr "|   "
      putStr (show (fst x))
      putStrLn "\n") indexed_board
  print_wall "-"
  print_x_coords

change list x element= (fst (splitAt x list)) ++ [element] ++ (tail (snd (splitAt x list)))

change_board board x y element = (fst (splitAt y board)) ++ [(change (board !! y) x element)] ++ (tail (snd (splitAt y board)))

get_square board x y = ((board !! y) !! x)


x_coords 'a' = 0
x_coords 'b' = 1
x_coords 'c' = 2
x_coords 'd' = 3
x_coords 'e' = 4
x_coords 'f' = 5
x_coords 'g' = 6
x_coords 'h' = 7

y_coords '1' = 7
y_coords '2' = 6
y_coords '3' = 5
y_coords '4' = 4
y_coords '5' = 3
y_coords '6' = 2
y_coords '7' = 1
y_coords '8' = 0
chess_pos pos = (x_coords (pos !! 0), y_coords (pos !! 1))

invert True = False
invert False = True

chess_prompt str = do
    putStr str
    hFlush stdout
    newstr <- getLine
    -- print newstr
    return (chess_pos newstr)

legal_move board x0 y0 x y color
  -- | (get_square board x0 y0) == (Empty, None) = False
  -- | ((get_square board x0 y0) == (Pawn, color) && (abs(x - x0) == 1)) = True
  -- | ((get_square board x0 y0) == (Pawn, color) && ((get_square board x y) == (Empty, None)) && ((y0 - y) == 1)) = True
  | otherwise = True

eventLoop b = do
  printboard b
  (x0, y0) <- chess_prompt "Move piece from: "
  (x, y) <- chess_prompt "To: "
  if (legal_move b x0 y0 x y W)
    then do
      clearScreen
      eventLoop (change_board (change_board b x y (get_square b x0 y0)) x0 y0 (Empty, None))
    else do
      clearScreen
      putStrLn ("\nIllegal Move!\n")
      eventLoop b

main = do
  clearScreen
  eventLoop (change (change (change (change (makeBoard (Empty, None)) 0 (getRow (Pawn, B))) 1 (getRow (Pawn, B))) 6 (getRow (Pawn, W))) 7 (getRow (Pawn, W)))
