module AIO2 
where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Utility

instance Ord (Side, Board) where
	compare (s1, b1) (s2, b2) = 
		compare (utility s1 b1) (utility s2 b2) 

-- MINIMAX

-- how to write compose2 ::  (a -> b -> c) -> (c -> d) -> (a -> b -> d)
minimaxDecision :: Side -> Board -> Board 
minimaxDecision side board = 
	maximum $ enumerate side board 


maxVal :: 
maxVal 


minVal ::
minVal 


--- MISC FUNCTIONS

utility :: Side -> Board -> Int 
utility side board 
	| won side board = 1
	| won (opposite side) board = -1
	| otherwise = 0 

enumerate :: Side -> Board -> [(Side, Board)]
enumerate side board =
	zip [side, side..] $ map (addMove side board) $ filter (posIsEmpty board) [0..8]

makeAImove :: Side -> Board -> Board 
makeAImove = minimaxDecision


--- DEBUGGING CODE

-- examples 
const_side :: Side 
const_side = 'I'

const_board0 :: board 
const_board0 = 
	Map.fromList 
	[(0 :: Int, '*'), (1 :: Int, '*'), (2 :: Int, '*'), 
	 (3 :: Int, '*'), (4 :: Int, '*'), (5 :: Int, '*'), 
	 (6 :: Int, '*'), (7 :: Int, '*'), (8 :: Int, '*')]

-- print result

main :: IO ()
main = do
	putStrLn $ "side: " ++ [const_side] ++ "\n"
	putStrLn $ "initial board: " ++ "\n"
	putStrLn $ prettyPrint const_board1 ++ "\n\n" ++ "-----" ++ "\n"
	let bestBoard = makeAImove const_side const_board0
	prettyPrint bestBoard




-- NOTES
-- i should program in literate haskell
-- Norvig's method is probably harder to debug 