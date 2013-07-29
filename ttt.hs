import Data.List

type Board = [[Char]]
type Side = Char

config = [['1', '2', '3'], 
		  ['4', '5', '6'], 
		  ['7', '8', '9']]

board = [['*', '*', '*'],
		 ['*', '*', '*'],
		 ['*', '*', '*']]

prettyPrint :: [[Char]] -> String
prettyPrint = ((:) ' ') . intersperse ' ' . intercalate "\n"

addMove :: Char -> Int -> [[Char]] -> [[Char]]
addMove side pos board = board

main :: IO ()
main = do
	putStrLn $ "welcome to tic-tac-tonad" ++ "\n"
	putStrLn "here's the configuration" 
	putStrLn $ prettyPrint config ++ "\n"
	putStrLn "here's the board"
	putStrLn $ prettyPrint board ++ "\n"
	xmove board

xmove :: Board -> IO ()
xmove board = do
	newBoard move 'X' board
	ymove 

ymove :: Board -> IO ()
ymove = do
	move 'Y' board
	xmove

-- gets move, makes new board with move, prints new board
move :: Char -> Board -> Board
move side = do
	putStrLn $ (side : "'s move. enter number of position")
	(pos :: Int) <- fmap read getLine
	let newBoard = addMove side pos board
	putStrLn $ prettyPrint newBoard ++ "\n"



-- todo: 
-- check if move space taken or out of bounds, loop
-- check for win
-- fix -XScopedTypeVariables
-- write AI


