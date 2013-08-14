module AIO4
where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Utility

--- MISC FUNCTIONS

-- initial guards equivalent to norvig's terminalTest(state)
-- utility scaled by number of moves of *terminal* board, so for example 'I' will move correctly in test case #3
utility :: SidedBoard -> Float
utility sidedBoard@(side, board)
	| won maxSide board = traceShow ("terminal board: " ++ prettyPrintDebug board ++ "   side: " ++ [side] ++ "    utility: " ++ "1/m") (1.0 / (fromIntegral $ numMoves board)) 
	| won minSide board = traceShow ("terminal board: " ++ prettyPrintDebug board ++ "   side: " ++ [side] ++ "    utility: " ++ "-1/m") (-1.0 / (fromIntegral $ numMoves board))
	| full board = traceShow ("terminal full board: " ++ prettyPrintDebug board ++ "   side: " ++ [side] ++ "    utility: " ++ "0") 0.0
	| otherwise = 
		traceShow ("board:          " ++ prettyPrintDebug board ++ "   side: " ++ [side] ++ "    utility: " ++ (show ultimateUtility)) 
			ultimateUtility -- questionable
		where ultimateUtility = utility $ minimaxDecision (opposite side, board)

-- wraps up / calls norvig's maxValue(state) or minValue(state) depending on side
decideFunc :: Side -> ([SidedBoard] -> SidedBoard)
decideFunc side 
	| side == 'I' = maximumBy compareUtility
	| otherwise = minimumBy compareUtility -- 'O'
	where compareUtility sb1 sb2 = compare (utility sb1) (utility sb2) -- questionable

-- MINIMAX

-- equivalent to norvig's minimaxDecision(state)
minimaxDecision :: SidedBoard -> SidedBoard
minimaxDecision sidedBoard@(side, board) = 
	traceShow (">> chosen board: " ++ prettyPrintDebug chosenBoard ++ "   chosen side: " ++ [chosenSide]) result
	where result@(chosenSide, chosenBoard) = decideFunc side $ enumerate sidedBoard

-- hardcode "move in the middle"; otherwise very slow and will go in top left corner
makeAImove :: Side -> Board -> Board 
makeAImove side board 
	| empty board = addMove side board 4
	| otherwise = snd $ minimaxDecision (side, board) 


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
	-- account for / ignore symmetric boards (AI plays differently, hmm)
	-- choose first "win" board (such as in example.hs) or figure out effects of picking "max" (i.e. first winning board)
	-- alpha-beta pruning


