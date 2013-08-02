module AIO 
where
	-- export all (to specify: (fname, ..., fname))

import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Utility

--data MaxTree a = Empty | (MinTree a) (MinTree a)
--data MinTree a = Empty | (MaxTree a) (MaxTree a)

--data MaxTree Board = EmptyTree | Node Board {- # MinTrees = # empty spaces on board -}
-- or i could put an EmptyTree with a minimax value of -inf
--data MinTree Board = EmptyTree | Node Board 

data MaxTree b v = EmptyMaxTree | MaxNode b v (MinTree b v) deriving (Eq, Show)
data MinTree b v = EmptyMinTree | MinNode b v [MaxTree b v] deriving (Eq, Show)
-- the number of children should really be parametrized by board size
-- but i don't think it's the type's responsibility to deal with that

-- this is totally wrong btw. side parameters? size? recursion?
-- maybe the mintree should just be one -- after all, there should only be one path
-- maybe it should really be a linked list of maxBoard and minBoard
maxTree :: Side -> Board -> MaxTree Board Int
maxTree side board = 
	| full board = MaxNode board (calcMinimax board) [EmptyMinTree]
	| otherwise = MaxNode board (pickMax $ maxTree ) 

-- this is totally wrong btw. side parameters? size? recursion?
minTree :: Side -> Board -> MinTree Board Int
minTree side board = 
	| 

calcMinimax :: Board -> Int
calcMinimax board = 

-- given a MaxTree, picks the board of maximum minimax value in the next ply (of MinTrees)
pickMax :: MaxTree Board Int -> Board
pickMax (MaxNode board value mintrees) =
	-- go through mintrees, compare by value, return board of max value 

-- should work for general -- playing maximizing and minimizing side
-- side doesn't matter -- AI by default will always play the maximizer
makeAImove :: Side -> Board -> Board
makeAImove side board = 
	pickMax side $ maxTree side board 


-- insert in random/first free space
makeBadAImove :: Side -> Board -> Board
makeBadAImove side board = 
	Map.alter f pos board
		where f _ = Just side;
			  pos = (!! 0) $ Map.keys $ Map.filter isEmpty board
