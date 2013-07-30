import Data.List
import qualified Data.Map as Map
import Data.Maybe

{-
config = [['0', '1', '2'], 
		  ['3', '4', '5'], 
		  ['6', '7', '8']]

board = [['*', '*', '*'],
		 ['*', '*', '*'],
		 ['*', '*', '*']]
-}

type Board = Map.Map Int Char
--type Three = Map.Map String [Int]
type Three = [Int]
type Side = Char
--type Position = Int

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

-- maps have strange update / alter functions
addMove :: Side -> Int -> Board -> Board
addMove side pos board 
	= Map.alter f pos board
		where f _ = Just side

won :: Side -> Board -> Bool
won side board = 
	any (conqueredBy side board) $ Map.elems threes

conqueredBy :: Side -> Board -> Three -> Bool
conqueredBy side board =
	and . map (\ pos -> 
				(fromMaybe '-' $ Map.lookup pos board) == side)

----------

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
	putStrLn $ "\n" ++ prettyPrint newBoard ++ "\n"

	if won 'I' newBoard
		then do
			putStrLn "game over. input won!"
		else do
			omove (return newBoard)

omove :: IO Board -> IO ()
omove board = do
	newBoard <- getMove 'O' board
	putStrLn $ "\n" ++ prettyPrint newBoard ++ "\n"

	if won 'O' newBoard
		then do
			putStrLn "output won!"
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
			putStrLn $ "\n" ++ "error: out of bounds. too impure of a move" ++ "\n"
			getMove side board
		else if checkMove == 'I' || checkMove == 'O'
			then do
				putStrLn $ "\n" ++ "error: moved in an occupied space. too impure of a move" ++ "\n"
				getMove side board
			else do -- valid move
				let newBoard = addMove side pos boardResult;
				return newBoard

	


-- todo: 
-- write AI 
-- write peanut gallery

-- fix display so it aligns in terminal (or use gloss)
-- fix -XScopedTypeVariables?
-- learn about do notation (bind, why considered harmful)
-- n-tac-toe


