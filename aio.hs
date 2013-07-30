module AIO 
where
	-- export all (to specify: (fname, ..., fname))

import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Utility

-- insert in random/first free space
makeAImove :: Side -> Board -> Board
makeAImove side board = 
	Map.alter f pos board
		where f _ = Just side;
			  pos = (!! 0) $ Map.keys $ Map.filter isEmpty board
