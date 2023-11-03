import Data.Ratio ((%), Ratio)
import Data.Tuple (swap)
import Data.List ()
import Data.List.Split ()
import Data.Maybe ( catMaybes )
import Debug.Trace ()

data Player = PlayerOne | PlayerTwo deriving (Eq, Show)

type Point = (Int, Int)
data Direction = Rght | Down deriving (Eq, Show)
type Line = (Point, Direction)
data Move = Move Line deriving (Eq, Show)

data Box = Box Point Player deriving (Eq, Show) -- top left point of box and who controls it

type GameState = (Player, [Move], [Box]) --whose turn it is, list of moves done, list of boxes completed

type Winner = Maybe Player

--starting out 5x4 boxes

-- read in data
    -- parse it to our data types

-- check to see if move is legal
    -- check if move has been done before
    -- check if move goes out of bounds

-- make move
    -- add to list of moves
    -- check if box is filled
        -- add to list of boxes
    -- check if game is done
        --compute points from list of boxes

-- output
    --print each game state
    --print out winner
    --stop reading in stuff

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
-- ***need to check if move is valid before***
makeMove :: GameState -> Move -> GameState
makeMove (trn, mvs, bxs) (Move ((x, y), Rght)) = let upBox = checkBoxUp (trn, mvs, bxs) (Move ((x, y), Rght))
                                                     downBox = checkBoxDown (trn, mvs, bxs) (Move ((x, y), Rght))
                                                     newBoxes = catMaybes [upBox, downBox]
                                                     next = if trn == PlayerOne then PlayerTwo else PlayerOne
                                                 in (next, Move ((x, y), Rght):mvs, newBoxes ++ bxs)

makeMove (trn, mvs, bxs) (Move ((x, y), Down)) = let leftBox = checkBoxLeft (trn, mvs, bxs) (Move ((x, y), Down))
                                                     rightBox = checkBoxRight (trn, mvs, bxs) (Move ((x, y), Down))
                                                     newBoxes = catMaybes [leftBox, rightBox]
                                                     next = if trn == PlayerOne then PlayerTwo else PlayerOne
                                                 in (next, Move ((x, y), Down):mvs, newBoxes ++ bxs)
                                                       
                                                   