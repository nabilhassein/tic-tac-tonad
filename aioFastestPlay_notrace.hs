module AIOfastestPlay_notrace
where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Utility

--- MISC FUNCTIONS

-- initial guards equivalent to norvig's terminalTest(state)
utility :: SidedBoard -> Int
utility sidedBoard@(side, board)
	| won maxSide board = 1 
	| won minSide board = -1
	| full board = 0
	| otherwise = utility $ minimaxDecision (opposite side, board)

-- manual left fold that terminates upon finding the first winning board (as opposed to finding the board with highest utility among all)
-- works because utilities are only -1, 0, 1
-- doesn't seem any faster, though
decideBoard :: Side -> [SidedBoard] -> SidedBoard
decideBoard _ [board] = board
decideBoard maxSide (b:bs)
	| headUtility == 1 = b 
	| headUtility >= tailUtility = b
	| otherwise = tailBestBoard
		where headUtility = utility b;
			  tailUtility = utility tailBestBoard;
			  tailBestBoard = decideBoard maxSide bs
decideBoard minSide (b:bs)
	| headUtility == -1 = b 
	| headUtility <= tailUtility = b
	| otherwise = tailBestBoard
		where headUtility = utility b;
			  tailUtility = utility tailBestBoard;
			  tailBestBoard = decideBoard minSide bs

-- MINIMAX

-- equivalent to norvig's minimaxDecision(state)
minimaxDecision :: SidedBoard -> SidedBoard
minimaxDecision sidedBoard@(side, board) = 
	decideBoard side $ enumerate sidedBoard

-- hardcode "move in the middle"; otherwise very slow and will go in top left corner
makeAImove :: Side -> Board -> Board 
makeAImove side board 
	{-| empty board = addMove side board 4
	| otherwise -} = snd $ minimaxDecision (side, board) 


--- DEBUGGING CODE

-- examples 
-- fix this specification later
maxSide :: Side 
maxSide = 'I'

minSide :: Side 
minSide = 'O'

const_side :: Side 
const_side = 'O'

const_board0 :: Board 
const_board0 = 
	Map.fromList 
	[(0 :: Int, 'O'), (1 :: Int, 'I'), (2 :: Int, '*'), 
	 (3 :: Int, 'O'), (4 :: Int, '*'), (5 :: Int, '*'), 
	 (6 :: Int, 'O'), (7 :: Int, 'I'), (8 :: Int, '*')]

const_board1 :: Board 
const_board1 = 
	Map.fromList 
	[(0 :: Int, 'O'), (1 :: Int, 'I'), (2 :: Int, '*'), 
	 (3 :: Int, '*'), (4 :: Int, 'I'), (5 :: Int, '*'), 
	 (6 :: Int, 'O'), (7 :: Int, 'I'), (8 :: Int, '*')]

const_board2 :: Board 
const_board2 = 
	Map.fromList 
	[(0 :: Int, '*'), (1 :: Int, '*'), (2 :: Int, '*'), 
	 (3 :: Int, '*'), (4 :: Int, 'I'), (5 :: Int, '*'), 
	 (6 :: Int, 'O'), (7 :: Int, 'O'), (8 :: Int, '*')]
-- O plays incorrectly

const_board3 :: Board 
const_board3 = 
	Map.fromList 
	[(0 :: Int, 'O'), (1 :: Int, 'I'), (2 :: Int, '*'), 
	 (3 :: Int, 'I'), (4 :: Int, 'I'), (5 :: Int, '*'), 
	 (6 :: Int, '*'), (7 :: Int, 'O'), (8 :: Int, 'I')]
-- compare O in top right corner or bottom left corner vs. no O there

-- print result

mainx :: IO ()
mainx = do
	putStrLn $ "side: " ++ [const_side] ++ "\n"
	putStrLn $ "initial board: " ++ "\n"
	putStrLn $ prettyPrint const_board3
	 ++ "\n\n" ++ "-----" ++ "\n"
	let bestBoard = makeAImove const_side const_board3
	putStrLn $ prettyPrint bestBoard




-- NOTES
-- i should program in literate haskell 
-- implement data Side = I | O
-- optimizations:
	-- negamax
	-- account for / ignore symmetric boards (AI plays differently, hmm)
	-- alpha-beta pruning


