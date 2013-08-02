module AIO1 
where

import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Utility

--- there seems to be a lot of boilerplate..
-- why don't you finish this, then generalize it

data MaxList b v = EmptyMaxBoard | MaxBoard b v (MinList b v) deriving (Eq, Show)
data MinList b v = EmptyMinBoard | MinBoard b v (MaxList b v) deriving (Eq, Show)

instance (Eq b, Ord v) => Ord (MaxList b v) where
	compare EmptyMaxBoard _ = LT 
	compare _ EmptyMaxBoard = GT
	compare l1@(MaxBoard _ v1 _) l2@(MaxBoard _ v2 _) = compare v1 v2

instance (Eq b, Ord v) => Ord (MinList b v) where
	compare EmptyMinBoard _ = LT 
	compare _ EmptyMinBoard = GT
	compare l1@(MinBoard _ v1 _) l2@(MinBoard _ v2 _) = compare v1 v2

-- ah! top node is *current* board!
-- side?
-- remove the minTree $ part?
maxTree :: Side -> Board -> MaxList Board Int 
maxTree side board 
	| full board = MaxBoard board (minimaxVal side board) EmptyMinBoard
	| otherwise = MaxBoard board recursiveVal recursiveList -- it's not complaining about this board?
		where recursiveVal = minimaxVal side (boardOf recursiveList);
			  boardOf (MinBoard b v maxlist) = b; 
			  recursiveList = {-minTree $ -}
			  	maxInPly $ map (\ (s, b) -> minTree s b) $ enumerate side board
			  	-- switched minInPly -> maxInPly

	-- needs to be of type MinList b v
	-- make the actual minTree the minTree with the greatest value
	-- enumerate all the possible moves, map minTree on them, 
	-- probably making 9999 mistakes rn
	-- hmm.. how much state/structure to store in functions and passing stuff?
	-- how much in a data structure?

-- can't tell if this works??
-- reverse sides
-- i need to figure out how to refactor this -- call with opposite side, min max function???

minTree :: Side -> Board -> MinList Board Int 
minTree side board 
	| full board = MinBoard board (minimaxVal side board) EmptyMaxBoard
	| otherwise = MinBoard board recursiveVal recursiveList
		where recursiveVal = minimaxVal side (boardOf recursiveList);
			  boardOf (MaxBoard b v minlist) = b;
			  recursiveList = {- maxTree $ -}
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

-- lists can't be empty..
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
-- should side even be a parameter?
makeAImove :: Side -> Board -> Board
makeAImove side board = 
	maxBoard $ maxTree side board 

-- insert in random/first free space
makeBadAImove :: Side -> Board -> Board
makeBadAImove side board = 
	Map.alter f pos board
		where f _ = Just side;
			  pos = (!! 0) $ Map.keys $ Map.filter isEmpty board
