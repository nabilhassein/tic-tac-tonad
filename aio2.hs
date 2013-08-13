module AIO2 
where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Utility


type SidedBoard = (Side, Board)

--- MISC FUNCTIONS

-- are sides/boards being played and enumerated and utility-asssessed correctly?
-- utility isn't reversing properly, what matters is the maxSide *and* the current side
-- **fix this or rewrite code again
-- (t, t)=t, (t, f)=f, (f,t)=f, (f,f)=t
utility :: SidedBoard -> Int
utility sidedBoard@(side, board)
	| side == maxSide && won maxSide board = 1
	| side == maxSide && won minSide board = -1
	| side == minSide && won minSide board = 1
	| side == minSide && won maxSide board = -1
	| full board = 0 
	| otherwise = utility $ minimaxDecision (opposite side, board)

decideFunc :: Side -> ([SidedBoard] -> SidedBoard)
decideFunc side 
	| side == 'I' = maximumBy compareUtility
	| otherwise = minimumBy compareUtility -- 'O'
	where compareUtility sb1 sb2 = compare (utility sb1) (utility sb2)

enumerate :: SidedBoard -> [SidedBoard]
enumerate (side, board) =
	zip [side, side..] $ map (addMove side board) $ filter (posIsEmpty board) [0..8]

-- MINIMAX

-- how does this behave on a finished board? maybe that's why utility should be here
minimaxDecision :: SidedBoard -> SidedBoard
minimaxDecision sidedBoard@(side, board) = 
	decideFunc side $ enumerate sidedBoard

makeAImove :: Side -> Board -> Board 
makeAImove side board = snd $ minimaxDecision (side, board) 


--- DEBUGGING CODE

-- examples 
-- fix this specification later
maxSide :: Side 
maxSide = 'I'

minSide :: Side 
minSide = 'O'

const_side :: Side 
const_side = 'I'

const_board0 :: Board 
const_board0 = 
	Map.fromList 
	[(0 :: Int, 'I'), (1 :: Int, 'I'), (2 :: Int, 'O'), 
	 (3 :: Int, 'O'), (4 :: Int, 'O'), (5 :: Int, 'I'), 
	 (6 :: Int, 'I'), (7 :: Int, 'I'), (8 :: Int, 'O')]

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