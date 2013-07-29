import Data.List

type Board = [[Char]]
type Side = Char
type Position = Int

config = [['1', '2', '3'], 
		  ['4', '5', '6'], 
		  ['7', '8', '9']]

board = [['*', '*', '*'],
		 ['*', '*', '*'],
		 ['*', '*', '*']]

prettyPrint :: Board -> String
prettyPrint = ((:) ' ') . intersperse ' ' . intercalate "\n"

addMove :: Side -> Position -> Board -> Board
addMove side pos board = board

main :: IO ()
main = do
	putStrLn $ "welcome inside the tic-tac-tonad" ++ "\n"
	putStrLn "here are the position numbers" 
	putStrLn $ prettyPrint config ++ "\n"
	putStrLn "here's the board"
	putStrLn $ prettyPrint board ++ "\n"
	xmove (return board)

xmove :: IO Board -> IO ()
xmove board = do
	newBoard <- move 'X' board
	putStrLn $ prettyPrint newBoard ++ "\n"
	-- i don't like having the printing in both functions
	-- but i did promise that main :: IO ()
	ymove (return newBoard)

ymove :: IO Board -> IO ()
ymove board = do
	newBoard <- move 'Y' board
	putStrLn $ prettyPrint newBoard ++ "\n"
	xmove (return newBoard)

-- gets move, makes new board with move
-- does not print new board
move :: Side -> IO Board -> IO Board 
move side board = do
	putStrLn $ (side : "'s move. enter number of position")
	(pos :: Int) <- fmap read getLine
	boardResult <- board 
	-- can i fix this? lots of wrapping, unwrapping boards
	let newBoard = addMove side pos boardResult
	return newBoard

	



-- todo: 
-- check if move space taken or out of bounds, loop
-- check for win
-- fix -XScopedTypeVariables
-- write AI


