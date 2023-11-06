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

data Winner = Winner Player | Draw

--starting out 5x4 boxes
sizeX = 6
sizeY = 5
numBoxes = 20

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

--checks if move is out of bounds
checkOutOfBounds :: GameState -> Move -> Bool
checkOutOfBounds _ (Move ((x, _), Rght)) = not ((x < 0) || (x >= sizeX))
checkOutOfBounds _ (Move ((_, y), Down)) = not ((y < 0) || (y >= sizeY))

--checks if move has been done before
checkIfDone :: GameState -> Move -> Bool
checkIfDone (_, mvs, _) x = not (x `elem` mvs)


--checks if move is legal
checkLegal :: GameState -> Move -> Bool
checkLegal (trn, mvs, bxs) (Move ((x, y), dir)) = if (checkOutOfBounds (trn, mvs, bxs) (Move ((x, y), dir)) && checkIfDone (trn, mvs, bxs) (Move ((x, y), dir))) then True else False

-- updates gamestate with move
-- ***need to check if move is valid before***
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
 
--checks if there is a winner
checkWinner :: GameState -> Maybe Winner
checkWinner (trn, mvs, bxs) = if (length bxs >= numBoxes) then Just (calculateScore (trn, mvs, bxs) 0 0) else Nothing

--calculate then final score
calculateScore :: GameState -> Int -> Int -> Winner
calculateScore (_, _, []) p1score p2score = if p1score > p2score then Winner PlayerOne else if p2score > p1score then Winner PlayerTwo else Draw
calculateScore (trn, mvs, ((Box _ player):xs)) p1score p2score = if player == PlayerOne then calculateScore (trn, mvs, xs) (p1score+1) p2score else calculateScore (trn, mvs, xs) p1score (p2score+1)



--creates a list of all legal moves
createAllMoves :: Int -> Int -> [Move]
createAllMoves (-1) (-1) = []
createAllMoves x y = (Move ((x,y), Down)):(Move ((x,y), Rght)):(createAllMoves (x-1) (y-1))

--finds all the possible legal moves
findAllLegalMoves :: GameState -> [Move]
findAllLegalMoves (trn, mvs, bxs) = let lst = createAllMoves sizeX sizeY in [x | x <- lst, checkLegal (trn, mvs, bxs) x]