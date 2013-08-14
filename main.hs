import Data.List
import qualified Data.Map as Map
import Data.Maybe

import AIOfastestPlay_notrace
import Utility

main :: IO ()
main = do
	putStrLn $ "welcome inside the tic-tac-tonad" 
	putStrLn $ "you must now fight an impure battle of I and O" ++ "\n"
	putStrLn $ "here are the position numbers" ++ "\n"
	putStrLn $ prettyPrint config ++ "\n"
	putStrLn $ "here's the board" ++ "\n"
	putStrLn $ prettyPrint initialBoard ++ "\n"
	imove (return config)

imove :: IO Board -> IO ()
imove board = do
	newBoard <- getMove 'I' board
	putStrLn $ prettyPrint newBoard ++ "\n"

	if won 'I' newBoard
		then do
			putStrLn "game over. 'I' won! ;)"
		else if full newBoard
			then do
				putStrLn "draw. I and O live in harmony"
			else do
				omove (return newBoard)

omove :: IO Board -> IO ()
omove board = do
	newBoard <- getAIMove 'O' board
	putStrLn $ "\n" ++ prettyPrint newBoard ++ "\n"

	if won 'O' newBoard
		then do
			putStrLn "game over. 'O' won!"
		else if full newBoard
			then do
				putStrLn "draw. I and O live in harmony"
			else do
				imove (return newBoard)

-- gets move, makes new board with move
-- does not print new board
getMove :: Side -> IO Board -> IO Board 
getMove side board = do
	putStrLn $ (side : "'s move. enter number of position")
	(pos :: Int) <- fmap read getLine
	boardResult <- board 

	let value = Map.lookup pos boardResult;
		checkMove = fromMaybe '-' value

	if checkMove == '-'
		then do
			putStrLn $ "\n" ++ "error: out of bounds" ++ "\n"
			getMove side board
		else if not $ isEmpty checkMove
			then do
				putStrLn $ "\n" ++ "error: moved in an occupied space" ++ "\n"
				getMove side board
			else do -- valid move
				let newBoard = addMove side boardResult pos;
				return newBoard

getAIMove :: Side -> IO Board -> IO Board
getAIMove side board = do
	putStrLn $ "AI/O is thinking" 
	boardResult <- board
	let newBoard = makeAImove side boardResult
	return newBoard	



-- todo: 

-- rock paper scissors AI
-- game of life, genetic algorithm
-- write peanut gallery
-- think up n-tac-toe, pentago, or other variants
-- "Terni Lapilli: each player has 3 pieces & had to move them around to empty spaces"
-- not related, but: chatbot

-- fix display so it aligns in terminal (or use gloss) (or prettyPrint like example.hs)
-- fix -XScopedTypeVariables?


