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

-- side?
maxTree :: Side -> Board -> MaxList Board Int 
maxTree side board = 
	| full board = MaxBoard board (minimaxVal side board) (EmptyMinBoard)
	| otherwise = MaxBoard board recursiveVal recursiveList
		where recursiveVal = minimaxVal $ board recursiveList
			  board (MinBoard b v maxlist) = b
			  recursiveList = minTree $ 
			  	minInPly $ map minTree $ enumerate side board 
	-- needs to be of type MinList b v
	-- make the actual minTree the minTree with the greatest value
	-- enumerate all the possible moves, map minTree on them, 
	-- probably making 9999 mistakes rn
	-- hmm.. how much state/structure to store in functions and passing stuff?
	-- how much in a data structure?

-- can't tell if this works??????
minTree :: Side -> Board -> MinList Board Int 
minTree side board = 
	| full board = MinBoard board (minimaxVal side board) (EmptyMaxBoard)
	| otherwise = MinBoard board recursiveVal recursiveList
		where recursiveVal = minimaxVal $ board recursiveList
			  board (MaxBoard b v minlist) = b
			  recursiveList = maxTree $ 
			  	maxInPly $ map maxTree $ enumerate side board 

--

-- the work happens here
minimaxVal :: Side -> Board -> Int 
miniMaxval side board =


-- for each free space, put that side piece in the board
enumerate :: Side -> Board -> [Board]
enumerate side board =

---

maxInPly :: 


minInPly ::


-- side?
maxBoard :: MaxList Board Int -> Board 
-- should not be passed an empty one of either kind (at least one move left)
maxBoard (MaxBoard maxb maxv (MinBoard minb minv maxlist)) = minb 

minBoard :: MinList Board Int -> Board
minBoard (MinBoard minb minv (MaxBoard maxb maxv minlist)) = maxb

--

maxVal :: MaxList Board Int -> Int
maxBoard (MaxBoard maxb maxv (MinBoard minb minv maxlist)) = minv

minVal :: MinList Board Int -> Int
minBoard (MinBoard minb minv (MaxBoard maxb maxv minlist)) = maxv

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
