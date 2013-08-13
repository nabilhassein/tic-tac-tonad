{-   
  A Haskell tic tac toe game using minimax searching.
  Written as an example program for CISC 260, Winter 2006.
  
  author: M. Lamb
-}

{-
  Step 1: Representation for game states.  We'll use an
  algebraic type to encapsulate:
    - whose turn it is (true for X, false for O)
    - the list of squares x is occupying (in order)
    - the list of squares o is occupying (in order)
-}
data State = State Bool [Int] [Int]
  deriving (Show, Eq)

-- an arbitrary state that can never be reached in a real game.
-- used to flag errors or when the state doesn't matter (when
-- we used a wildcard in Prolog)
arbState :: State
arbState = State True [99] [99]

-- some general helper predicates

-- extract the player from a game state
player :: State -> Bool
player (State p _ _) = p

-- adds an item to an ordered list; result must be an ordered list
addToList :: [Int] -> Int -> [Int]
addToList [] x = [x]
addToList (x:xs) y
  | y <= x = y:x:xs
  | otherwise = x : (addToList xs y)
  
-- checks if an ordered list of numbers is a subset of another ordered list.
-- In other words, ordSubset(s1,s2) is true if every element of s1 is also
-- in s2.
ordSubset :: [Int] -> [Int] -> Bool
ordSubset [] _ = True
ordSubset _ [] = False
ordSubset (x:xs) (y:ys)
  | x == y = ordSubset xs ys
  | x > y = ordSubset (x:xs) ys
  | otherwise = False
  
{-
  Step 2: I/O program for displaying a tic tac toe board
-}
-- displayState state displays the tic tac toe board for a game state
displayState :: State -> IO ()
displayState (State _ xlist olist) =
  do
    displayRow 1 xlist olist
    putStr "   ---------\n"
    displayRow 4 xlist olist
    putStr "   ---------\n"
    displayRow 7 xlist olist

-- displayRow start xlist olist displays a row of the tic tac toe board.
-- start gives the number of the first square in the row
displayRow :: Int -> [Int] -> [Int] -> IO ()
displayRow start xlist olist =
  do
    putStr "   " -- tab three spaces
    displaySquare start xlist olist
    putStr "| "
    displaySquare (start+1) xlist olist
    putStr "| "
    displaySquare (start+2) xlist olist
    putStr "\n"

-- displaySquare S Xlist Olist displays a single square in the board,
-- give lists of what squares X and O are occupying
displaySquare :: Int -> [Int] -> [Int] -> IO ()
displaySquare sq xlist olist
  | elem sq xlist = putStr "X "
  | elem sq olist = putStr "O "
  | otherwise = putStr ((show sq) ++ " ")
  
{-
  Step 3: Evaluate terminal states.
  In Prolog, we had a "terminal" predicate, which failed if the state was not terminal
  Here, it seems more natural to have three predicates, checking for the three kinds
  of terminal states.
-}

-- a list of the 8 winning combinations
winningCombs :: [[Int]]
winningCombs = [[1,2,3],[4,5,6],[7,8,9],[1,4,7],[2,5,8],[3,6,9],[1,5,9],[3,5,7]]

-- test if X or O wins
xWins :: State -> Bool
xWins (State _ xlist _) = winner xlist
oWins :: State -> Bool
oWins (State _ _ olist) = winner olist
winner :: [Int] -> Bool
winner list = or [ordSubset comb list | comb <- winningCombs]

-- test if game is a draw (assuming nobody has won)
draw (State _ xlist olist) = length xlist + length olist == 9

{-
  Step 4: Specify the legal moves from a State.  moves st = list of all legal
  states that can occur one move after st.
-}
-- optimization: only three logically different moves on an empty board
moves (State True [] []) = 
  [(State False [1] []), (State False [2] []), (State False [5] [])]
moves (State False [] []) =
  [(State True [] [1]), (State True [] [2]), (State True [] [5])]
moves (State True xlist olist) = -- X's turn
  [(State False (addToList xlist square) olist) | 
    square <- [1..9], not (elem square xlist), not (elem square olist)]
moves (State False xlist olist) = -- O's turn
  [(State True xlist (addToList olist square)) |
    square <- [1..9], not (elem square xlist), not (elem square olist)]
    
{-
  Step 5: Minimax evaluation.
  2 = X wins
  1 = draw
  0 = O wins
  Result of minimax is a tuple: (best move, value of best move)
-}
-- reverses the players in the moves function
minimax :: State -> (State, Int)
minimax state
  | xWins state = (arbState, 2)
  | oWins state = (arbState, 0)
  | draw state = (arbState, 1) -- utility in minimax
  | otherwise = bestMove (player state) (moves state) -- ?
  -- best move for the current player of the enumerate (opposite player) state
  
-- choose the best move from a list
-- Parameters: 
--   1. player (true for X, false for O)
--   2. list of states
-- Result = tuple of 
--   1. best state in the list
--   2. value of the best state
bestMove :: Bool -> [State] -> (State, Int)
bestMove _ [state] = (state, stateVal state) -- ?
bestMove True (state1:tail) 
  | val1 == 2 = (state1,val1) -- may be slightly better than a maxBy (ends early) 
  | val1 >= tailVal = (state1,val1)
  | otherwise = (tailBest,tailVal)
  where 
    val1 = stateVal state1 -- minimax call with current side
    (tailBest,tailVal) = bestMove True tail
bestMove False (state1:tail) 
  | val1 == 0 = (state1,val1)
  | val1 <= tailVal = (state1,val1)
  | otherwise = (tailBest,tailVal)
  where 
    val1 = stateVal state1 -- minimax call with current side
    (tailBest,tailVal) = bestMove False tail
    
-- just the value part of minimax
stateVal :: State -> Int
stateVal state = val
  where (_,val) = minimax state

{-
  Step 6: I/O Program to play the game.  Computer is always
  X and user is always O.
-}

-- ttt: X goes first
ttt :: IO ()
ttt = showAndPlay (State True [] [])

-- meFirst: O goes first
meFirst :: IO ()
meFirst = showAndPlay (State False [] [])

showAndPlay :: State -> IO ()
showAndPlay state =
  do
    displayState state
    play state

play :: State -> IO ()
play state 
  | xWins state = putStr "I WIN!"
  | oWins state = putStr "YOU WIN!"
  | draw state = putStr "This game is a draw."
  | player state = -- X's turn
    do
      putStr("searching for my move...\n")
      let (bestMove,_) = minimax state
      displayState bestMove
      play bestMove
  | otherwise = -- O's turn
    do
      putStr("enter your move: ")
      input <- getLine
      let square = (read input) :: Int
      let nextState = addO state square
      if (nextState == arbState) 
        then
          putStr "not a legal move"
        else do
          displayState nextState
          play nextState
      
main :: IO ()
main = play (State True [] [])

-- addO adds an O to a square.  If that's not a legal move, returns 
-- the special arbstate value.
addO :: State -> Int -> State
addO (State False xlist olist) square 
  | not (elem square xlist) && (elem square [1..9]) =
    (State True xlist (addToList olist square))
addO _ _ = arbState
      