module Utility where

import Data.List
import qualified Data.Map as Map
import Data.Maybe


b = Map.fromList 
	[(0 :: Int, 'I'), (1 :: Int, 'O'), (2 :: Int, '*'), 
	 (3 :: Int, 'I'), (4 :: Int, 'I'), (5 :: Int, '*'), 
	 (6 :: Int, '*'), (7 :: Int, 'O'), (8 :: Int, 'O')]

type Side = Char
type Three = [Int]
type Board = Map.Map Int Char
type SidedBoard = (Side, Board)

config :: Board
config = toBoard ['0'..'8']

initialBoard :: Board
initialBoard = toBoard $ replicate 9 '*'

threes :: Map.Map String Three
threes = Map.fromList
	[("row0", [0, 1, 2]),
	 ("row1", [3, 4, 5]),
	 ("row2", [6, 7, 8]),
	 ("col0", [0, 3, 6]),
	 ("col1", [1, 4, 7]),
	 ("col2", [2, 5, 8]),
	 ("diag0", [0, 4, 8]),
	 ("diag6", [6, 4, 2])]

toBoard :: [Char] -> Map.Map Int Char
toBoard = Map.fromList . zip [0..8]

-- refuse to use fold. make this shorter?
-- maybe write a function to convert to [Char] or [[Char]], 
-- then prettyPrint as before
prettyPrint :: Board -> String
prettyPrint board = 
	map snd $
	filter (inRange 0 2) list
	++ ((-1, '\n') : (filter (inRange 3 5) list))
	++ ((-1, '\n') : (filter (inRange 6 8) list))
		where list = Map.toList board;
			  inRange lower upper (k, v) = lower <= k && k <= upper

prettyPrintDebug :: Board -> String
prettyPrintDebug board = 
	map snd $
	filter (inRange 0 2) list
	++ ((-1, ' ') : (filter (inRange 3 5) list))
	++ ((-1, ' ') : (filter (inRange 6 8) list))
		where list = Map.toList board;
			  inRange lower upper (k, v) = lower <= k && k <= upper

posIsEmpty :: Board -> Int -> Bool
posIsEmpty board pos = isEmpty $ fromMaybe '-' $ Map.lookup pos board

isEmpty :: Char -> Bool
isEmpty val = val /= 'I' && val /= 'O'

numEmpty :: Board -> Int
numEmpty = 
	Map.fold (\ tile acc -> if isEmpty tile then acc + 1 else acc) 0

-- maps have strange update / alter functions
addMove :: Side -> Board -> Int -> Board
addMove side board pos =
	Map.alter f pos board
		where f _ = Just side

enumerate :: SidedBoard -> [SidedBoard]
enumerate (side, board) =
	zip [side, side..] $ map (addMove side board) $ filter (posIsEmpty board) [0..8]

won :: Side -> Board -> Bool
won side board = 
	any (conqueredBy side board) $ Map.elems threes

conqueredBy :: Side -> Board -> Three -> Bool
conqueredBy side board =
	and . map (\ pos -> 
				(fromMaybe '-' $ Map.lookup pos board) == side)

full :: Board -> Bool
full =
	and . map (not . isEmpty) . Map.elems 

empty :: Board -> Bool
empty = 
	and . map isEmpty . Map.elems

opposite :: Side -> Side
opposite 'I' = 'O'
opposite 'O' = 'I'








