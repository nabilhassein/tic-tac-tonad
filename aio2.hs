module AIO2 
where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Utility

-- would like to do: type SidedBoard = (Side, Board), type decideFunc = ? (possible?)
-- anyway, -XFlexibleInstances
instance Ord (Side, Board) where
	compare sb1 sb2 = 
		compare (utility sb1) (utility sb2) 

--- MISC FUNCTIONS

-- uh-oh. are sides/boards being played and enumerated and utility-asssessed correctly

utility :: (Side, Board) -> Int 
utility sidedBoard@(side, board)
	| won side board = 1
	| won (opposite side) board = -1
	| full board = 0 
	| otherwise = utility $ minimaxDecision (opposite side, board)

decideFunc :: (Ord a) => Side -> ([a] -> a)
decideFunc 'I' = maximum
decideFunc 'O' = minimum

enumerate :: (Side, Board) -> [(Side, Board)]
enumerate (side, board) =
	zip [side, side..] $ map (addMove side board) $ filter (posIsEmpty board) [0..8]

-- MINIMAX

minimaxDecision :: (Side, Board) -> (Side, Board)
minimaxDecision sidedBoard@(side, board) = 
	decideFunc side $ enumerate sidedBoard

makeAImove :: Side -> Board -> Board 
makeAImove side board = snd $ minimaxDecision (side, board) 


--- DEBUGGING CODE

-- examples 
const_side :: Side 
const_side = 'I'

const_board0 :: Board 
const_board0 = 
	Map.fromList 
	[(0 :: Int, 'I'), (1 :: Int, 'I'), (2 :: Int, '*'), 
	 (3 :: Int, '*'), (4 :: Int, '*'), (5 :: Int, '*'), 
	 (6 :: Int, '*'), (7 :: Int, '*'), (8 :: Int, '*')]

-- print result

main :: IO ()
main = do
	putStrLn $ "side: " ++ [const_side] ++ "\n"
	putStrLn $ "initial board: " ++ "\n"
	putStrLn $ prettyPrint const_board0
	 ++ "\n\n" ++ "-----" ++ "\n"
	let bestBoard = makeAImove const_side const_board0
	putStrLn $ prettyPrint bestBoard




-- NOTES
-- i should program in literate haskell
-- Norvig's method is probably harder to debug 