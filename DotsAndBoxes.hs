import Data.Ratio ((%), Ratio)
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.List.Split ()
import Data.Maybe ( catMaybes, isJust )
import Debug.Trace ()
import Text.XHtml (base)

data Player = PlayerOne | PlayerTwo deriving (Eq, Show)

type Point = (Int, Int)
data Direction = Rght | Down deriving (Eq, Show)
type Line = (Point, Direction)
data Move = Move Line deriving (Eq, Show)

data Box = Box Point Player deriving (Eq, Show) -- top left point of box and who controls it

type GameState = (Player, [Move], [Box]) --whose turn it is, list of moves done, list of boxes completed

data Winner = Winner Player | Draw deriving (Eq, Show)

--starting out 5x4 boxes
rows = 4
columns = 5
numBoxes = rows * columns

-- read in data
    -- parse it to our data types

-- check to see if move is legal                DONE
    -- check if move has been done before       DONE
    -- check if move goes out of bounds         DONE

-- make move
    -- add to list of moves                     DONE
    -- check if box is filled                   DONE
        -- add to list of boxes                 DONE

-- check if game is done                        DONE
    --compute points from list of boxes         DONE

-- output
    --print each game state
    --print out winner
    --stop reading in stuff

-- creates a list of all possible moves for an m x n board
createAllMoves :: Int -> Int -> [Move]
createAllMoves m n = [ Move ((x, y), Rght) | x <- [0..n-1], y <- [0..m] ] 
                  ++ [ Move ((x, y), Down) | x <- [0..n], y <- [0..m-1] ]

-- finds all the possible legal moves for a gamestate
findLegalMoves :: GameState -> [Move]
findLegalMoves (trn, mvs, bxs) = let allMoves = createAllMoves rows columns
                                 in [ move | move <- allMoves, move `notElem` mvs ]

-- checks if a move is legal
checkLegal :: GameState -> Move -> Bool
checkLegal game move = move `elem` findLegalMoves game

-- checks if upper box was made with right line
checkBoxUp :: GameState -> Move -> Maybe Box
checkBoxUp (trn, mvs, bxs) (Move ((x, y), Rght)) = if Move ((x, y+1), Rght) `elem` mvs 
                                                   && Move ((x, y+1), Down) `elem` mvs 
                                                   && Move ((x+1, y+1), Down) `elem` mvs 
                                                   then Just (Box (x, y+1) trn)
                                                   else Nothing
-- checks if lower box was made with right line
checkBoxDown :: GameState -> Move -> Maybe Box
checkBoxDown (trn, mvs, bxs) (Move ((x, y), Rght)) = if Move ((x, y-1), Rght) `elem` mvs 
                                                     && Move ((x, y), Down) `elem` mvs 
                                                     && Move ((x+1, y), Down) `elem` mvs 
                                                     then Just (Box (x, y) trn)
                                                     else Nothing
-- checks if left box was made with down line
checkBoxLeft :: GameState -> Move -> Maybe Box
checkBoxLeft (trn, mvs, bxs) (Move ((x, y), Down)) = if Move ((x-1, y), Rght) `elem` mvs 
                                                     && Move ((x-1, y), Down) `elem` mvs 
                                                     && Move ((x-1, y-1), Rght) `elem` mvs 
                                                     then Just (Box (x-1, y) trn)
                                                     else Nothing
-- checks if right box was made with down line
checkBoxRight :: GameState -> Move -> Maybe Box
checkBoxRight (trn, mvs, bxs) (Move ((x, y), Down)) = if Move ((x, y), Rght) `elem` mvs 
                                                      && Move ((x+1, y), Down) `elem` mvs 
                                                      && Move ((x, y-1), Rght) `elem` mvs 
                                                      then Just (Box (x, y) trn)
                                                      else Nothing

-- updates gamestate with move
makeMove :: GameState -> Move -> GameState
makeMove (trn, mvs, bxs) (Move ((x, y), Rght)) = if checkLegal (trn, mvs, bxs) (Move ((x, y), Rght)) then 
                                                 let upBox = checkBoxUp (trn, mvs, bxs) (Move ((x, y), Rght))
                                                     downBox = checkBoxDown (trn, mvs, bxs) (Move ((x, y), Rght))
                                                     newBoxes = catMaybes [upBox, downBox]
                                                     next = if trn == PlayerOne then PlayerTwo else PlayerOne
                                                 in (next, Move ((x, y), Rght):mvs, newBoxes ++ bxs)
                                                 else (trn, mvs, bxs)

makeMove (trn, mvs, bxs) (Move ((x, y), Down)) = if checkLegal (trn, mvs, bxs) (Move ((x, y), Rght)) then 
                                                 let leftBox = checkBoxLeft (trn, mvs, bxs) (Move ((x, y), Down))
                                                     rightBox = checkBoxRight (trn, mvs, bxs) (Move ((x, y), Down))
                                                     newBoxes = catMaybes [leftBox, rightBox]
                                                     next = if trn == PlayerOne then PlayerTwo else PlayerOne
                                                 in (next, Move ((x, y), Down):mvs, newBoxes ++ bxs)
                                                    else (trn, mvs, bxs)
 

-- checks if there is a winner and returns winner if so
checkWinner :: GameState -> Maybe Winner
checkWinner (trn, mvs, bxs) = if length bxs >= numBoxes 
                              then Just (calculateScore (trn, mvs, bxs) (0, 0)) 
                              else Nothing

-- calculates the final score and returns a winner or a draw
calculateScore :: GameState -> (Int, Int) -> Winner
calculateScore (_, _, []) (p1score, p2score)
  | p1score > p2score = Winner PlayerOne
  | p2score > p1score = Winner PlayerTwo
  | otherwise = Draw
calculateScore (trn, mvs, (Box _ player):bxs) (p1score, p2score) = if player == PlayerOne 
                                                                   then calculateScore (trn, mvs, bxs) (p1score+1, p2score) 
                                                                   else calculateScore (trn, mvs, bxs) (p1score, p2score+1)

--return horizontal line
printHorizontalLine :: GameState -> Int -> String
printHorizontalLine (trn, mvs, bxs) y = concat [ if Move ((x,(y `div` 2)), Rght) `elem` mvs then ".-" else ". " | x <- [0..columns]]

--return Vertical line
printVerticalLine :: GameState -> Int -> String
printVerticalLine (trn, mvs, bxs) y = let p = if trn == PlayerOne then "1" else "2" 
                                      in concat [ if Move ((x,(y `div` 2)), Down) `elem` mvs 
                                                  then if Box (x, (y `div` 2)) PlayerOne `elem` bxs then "|" ++ "1" 
                                                       else if Box (x, (y `div` 2)) PlayerTwo `elem` bxs then "|" ++ "2"
                                                       else "| " 
                                                  else "  " | x <- [0..columns]]


--return the game board
printGameBoard :: GameState -> String
--printGameBoard (trn, mvs, bxs) ((rows*2)+1) = if trn == PlayerOne then " Player One's Turn" else " Player Two's Turn"
printGameBoard (trn,mvs,bxs) = intercalate "\n" (show trn : [ if even y then printHorizontalLine (trn, mvs, bxs) y 
                                                              else printVerticalLine (trn, mvs, bxs) y | y <- [0..(rows*2)]])