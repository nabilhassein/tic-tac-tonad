module AIO1 
where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

import Utility

-- there seems to be a lot of boilerplate
-- finish this, then generalize it to a single List datatype

data MaxList b v = EmptyMaxTree | MaxBoard b v (MinList b v) deriving (Eq, Show)
data MinList b v = EmptyMinTree | MinBoard b v (MaxList b v) deriving (Eq, Show)

instance (Eq b, Ord v) => Ord (MaxList b v) where
	compare EmptyMaxTree _ = LT 
	compare _ EmptyMaxTree = GT
	compare l1@(MaxBoard _ v1 _) l2@(MaxBoard _ v2 _) = compare v1 v2

instance (Eq b, Ord v) => Ord (MinList b v) where
	compare EmptyMinTree _ = LT 
	compare _ EmptyMinTree = GT
	compare l1@(MinBoard _ v1 _) l2@(MinBoard _ v2 _) = compare v1 v2

maxTreeInit :: Side -> Board -> MaxList Board Int
maxTreeInit side board
	| full board = MaxBoard board (minimaxVal side board) EmptyMinTree
	| won side board = MaxBoard board 1 EmptyMinTree
	| won (opposite side) board = MaxBoard board (-1) EmptyMinTree
	| otherwise = MaxBoard board recursiveVal recursiveList 
		where recursiveVal = valueOf recursiveList;
			  valueOf (MinBoard b v maxlist) = v; 
			  recursiveList =
			  	maxInPly $ map (\ (s, b) -> minTree s b) $ enumerate side board

-- ah, top node is *current* board
-- wrong: top node should be *best* board
-- side?
-- remove the minTree $ part?
maxTree :: Side -> Board -> MaxList Board Int 
maxTree side board 
	| full board = MaxBoard board (minimaxVal side board) EmptyMinTree
	| won side board = MaxBoard board 1 EmptyMinTree
	| won (opposite side) board = MaxBoard board (-1) EmptyMinTree
	| otherwise = MaxBoard board recursiveVal recursiveList -- it's not complaining about this board?
		where recursiveVal = valueOf recursiveList;
			  valueOf (MinBoard b v maxlist) = v; 
			  recursiveList =
			  	maxInPly $ map (\ (s, b) -> minTree s b) $ enumerate (opposite side) board
			  	-- switched minInPly -> maxInPly

	-- needs to be of type MinList b v
	-- make the actual minTree the minTree with the greatest value
	-- enumerate all the possible moves, map minTree on them, 
	-- hmm.. how much state/structure to store in functions and passing stuff?
	-- how much in a data structure?

-- can't tell if this works
-- reverse sides
-- i need to figure out how to refactor this -- call with opposite side, min max function???

minTree :: Side -> Board -> MinList Board Int 
minTree side board 
	| full board = MinBoard board (minimaxVal side board) EmptyMaxTree
	| won side board = MinBoard board 1 EmptyMaxTree
	| won (opposite side) board = MinBoard board (-1) EmptyMaxTree
	| otherwise = MinBoard board recursiveVal recursiveList
		where recursiveVal = valueOf recursiveList;
			  valueOf (MaxBoard b v minlist) = v;
			  recursiveList =
			  	minInPly $ map (\ (s, b) -> maxTree s b) $ enumerate (opposite side) board
			  	-- switched maxInPly -> minInPly

--

-- the work happens here
minimaxVal :: Side -> Board -> Int 
minimaxVal side board 
	| won side board = 1
	| won (opposite side) board = -1
	| otherwise = 0 

-- for each free space, put that side piece in the board
-- edge cases?
enumerate :: Side -> Board -> [(Side, Board)]
enumerate side board =
	zip [side, side..] $ map (addMove side board) $ filter (posIsEmpty board) [0..8]

---

-- lists can't be empty
maxInPly :: Ord a => [a] -> a
maxInPly = maximum 


minInPly :: Ord a => [a] -> a
minInPly = minimum

-- side?
maxBoard :: MaxList Board Int -> Board 
-- should not be passed an empty one of either kind (at least one move left)
maxBoard (MaxBoard maxb maxv (MinBoard minb minv maxlist)) = minb 

minBoard :: MinList Board Int -> Board
minBoard (MinBoard minb minv (MaxBoard maxb maxv minlist)) = maxb

--

maxVal :: MaxList Board Int -> Int
maxVal (MaxBoard maxb maxv (MinBoard minb minv maxlist)) = minv

minVal :: MinList Board Int -> Int
minVal (MinBoard minb minv (MaxBoard maxb maxv minlist)) = maxv

---

-- AI always maximizes
-- should side be a parameter?
makeAImove :: Side -> Board -> Board
makeAImove side board = 
	maxBoard $ maxTreeInit side board 

const_side :: Side
const_side = 'I'

const_board0 :: Board 
const_board0 =
	Map.fromList 
	[(0 :: Int, '*'), (1 :: Int, '*'), (2 :: Int, '*'), 
	 (3 :: Int, '*'), (4 :: Int, '*'), (5 :: Int, '*'), 
	 (6 :: Int, '*'), (7 :: Int, '*'), (8 :: Int, '*')]

const_board1 :: Board 
const_board1 =
	Map.fromList 
	[(0 :: Int, 'O'), (1 :: Int, '*'), (2 :: Int, '*'), 
	 (3 :: Int, '*'), (4 :: Int, '*'), (5 :: Int, '*'), 
	 (6 :: Int, 'O'), (7 :: Int, 'I'), (8 :: Int, 'I')]

const_board2 :: Board 
const_board2 =
	Map.fromList 
	[(0 :: Int, 'I'), (1 :: Int, 'O'), (2 :: Int, '*'), 
	 (3 :: Int, 'I'), (4 :: Int, 'I'), (5 :: Int, '*'), 
	 (6 :: Int, '*'), (7 :: Int, 'O'), (8 :: Int, 'O')]

const_board3 :: Board 
const_board3 =
	Map.fromList 
	[(0 :: Int, '*'), (1 :: Int, '*'), (2 :: Int, '*'), 
	 (3 :: Int, '*'), (4 :: Int, 'O'), (5 :: Int, '*'), 
	 (6 :: Int, '*'), (7 :: Int, '*'), (8 :: Int, '*')]

-- DEBUGGING CODE
-- i should have written this first

main :: IO ()
main = do 
	putStrLn $ "side: " ++ [const_side] ++ "\n"
	putStrLn $ "initial board: " ++ "\n"
	putStrLn $ prettyPrint const_board1 ++ "\n\n" ++ "-----" ++ "\n"
	let tree = maxTreeInit const_side const_board1
	showMaxTree tree
-- should it be a MaxTree?

-- i should really refactor MaxList/MinList b v to be a generic node
-- **with side encoded in type? there are no sides here
showMaxTree :: MaxList Board Int -> IO ()
showMaxTree (MaxBoard board val minlist) = do
	putStrLn $ "value: " ++ (show val) ++ "\n"
	putStrLn $ "board: " ++ "\n"
	putStrLn $ prettyPrint board ++ "\n\n" ++ "-----" ++ "\n"
	showMinTree minlist
showMaxTree EmptyMaxTree = do
	putStrLn $ "EmptyMaxTree" ++ "\n"

-- all this duplicated code!
showMinTree :: MinList Board Int -> IO ()
showMinTree (MinBoard board val maxlist) = do
	putStrLn $ "value: " ++ (show val) ++ "\n"
	putStrLn $ "board: " ++ "\n"
	putStrLn $ prettyPrint board ++ "\n\n" ++ "-----" ++ "\n"
	showMaxTree maxlist
showMinTree EmptyMinTree = do
	putStrLn $ "EmptyMinTree" ++ "\n"

-- insert in random/first free space
makeBadAImove :: Side -> Board -> Board
makeBadAImove side board = 
	Map.alter f pos board
		where f _ = Just side;
			  pos = (!! 0) $ Map.keys $ Map.filter isEmpty board


-- to fix: 
-- moves are by sides "I, O, O, I, I, ..""
	-- fixed, but did "opposite side" hack in makeAImove
	-- fixed again? with maxTreeInit
-- fix efficiency

-- also, value of boards in list not assigned correctly
-- wait -- it's only the utilities of the *terminal states* that bubble up. 
	-- but it's already doing that
	-- no, it's not -- it's calculating the minimax value of the board, not taking its value

-- top node should be *best* board, not first board
-- no, top node should be *best next move*, not *best board*
-- no, actually top node should be first board, the next board should be best next move







