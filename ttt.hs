import Data.List
import qualified Data.Map as Map

type Board = Map.Map Int Char
type Side = Char
type Position = Int

{-
config = [['0', '1', '2'], 
		  ['3', '4', '5'], 
		  ['6', '7', '8']]

board = [['*', '*', '*'],
		 ['*', '*', '*'],
		 ['*', '*', '*']]
-}

toBoard :: [Char] -> Map.Map Int Char
toBoard = Map.fromList . zip [0..8]

config :: Board
config = toBoard ['0'..'8']

initialBoard :: Board
initialBoard = toBoard $ replicate 9 '*'

-- refuse to use fold. make this shorter?
-- maybe write a function to convert to [[Char]], then prettyPrint as before
prettyPrint :: Board -> String
prettyPrint board = 
	map snd $
	filter (inRange 0 2) list
	++ ((-1, '\n') : (filter (inRange 3 5) list))
	++ ((-1, '\n') : (filter (inRange 6 8) list))
		where list = Map.toList board;
			inRange lower upper (k, v) = lower <= k && k <= upper

-- maps have strange update / alter functions
addMove :: Side -> Position -> Board -> Board
addMove side pos board 
	= Map.alter f pos board
		where f _ = Just side

main :: IO ()
main = do
	putStrLn $ "welcome inside the tic-tac-tonad" ++ "\n"
	putStrLn $ "here are the position numbers" ++ "\n"
	putStrLn $ prettyPrint config ++ "\n"
	putStrLn $ "here's the board" ++ "\n"
	putStrLn $ prettyPrint initialBoard ++ "\n"
	xmove (return initialBoard)

xmove :: IO Board -> IO ()
xmove board = do
	newBoard <- getMove 'X' board
	putStrLn $ "\n" ++ prettyPrint newBoard ++ "\n"
	-- i don't like having the printing in both functions
	-- but i did promise that main :: IO ()
	omove (return newBoard)

omove :: IO Board -> IO ()
omove board = do
	newBoard <- getMove 'O' board
	putStrLn $ "\n" ++ prettyPrint newBoard ++ "\n"
	xmove (return newBoard)

-- gets move, makes new board with move
-- does not print new board
getMove :: Side -> IO Board -> IO Board 
getMove side board = do
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
-- board representation: ADT, [[Char]], Map Int Char?


