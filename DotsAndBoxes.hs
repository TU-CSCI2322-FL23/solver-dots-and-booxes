module DotsAndBoxes where
import Data.List (partition)
import Data.Maybe (mapMaybe, fromMaybe)

data Player = PlayerOne | PlayerTwo deriving (Eq, Show)

type Point = (Int, Int)
data Direction = Rght | Down deriving (Eq, Show, Ord)
type Line = (Point, Direction)
newtype Move = Move Line deriving (Eq, Show, Ord)

data Box = Box Point Player deriving (Eq, Show) -- top left point of box and who controls it

type GameState = (Player, [Move], [Box], (Int, Int)) -- whose turn it is, list of moves done, list of boxes completed, board size

data Winner = Winner Player | Draw deriving (Eq, Show)

-- initial gamestate with default 4x5 boxes
initGame = (PlayerOne, [], [], (4, 5)) :: GameState

startGame :: Int -> Int -> GameState
startGame rows cols = (PlayerOne, [], [], (rows, cols))

createAllMoves :: Int -> Int -> [Move]
createAllMoves m n = [ Move ((x, y), Rght) | x <- [0..n-1], y <- [0..m] ]
                  ++ [ Move ((x, y), Down) | x <- [0..n], y <- [0..m-1] ]

findLegalMoves :: GameState -> [Move]
findLegalMoves (trn, mvs, bxs, (rows, cols)) = let allMoves = createAllMoves rows cols
                                               in [ move | move <- allMoves, move `notElem` mvs ]

checkBounds :: GameState -> Move -> Bool
checkBounds (_, _, _, (rows, cols)) (Move ((x, y), Rght)) = x >= 0 && x < cols && y >= 0 && y <= rows
checkBounds (_, _, _, (rows, cols)) (Move ((x, y), Down)) = x >= 0 && x <= cols && y >= 0 && y < rows

checkLegal :: GameState -> Move -> Bool
checkLegal gs@(trn, mvs, bxs, sz) move = move `notElem` mvs && checkBounds gs move

turnSwap :: Player -> Player
turnSwap trn = if trn == PlayerOne then PlayerTwo else PlayerOne

subset :: Eq a => [a] -> [a] -> Bool
subset [] lst = True
subset (x:xs) lst
  | x `elem` lst = subset xs lst
  | otherwise    = False

checkBoxes :: GameState -> Move -> [Box]
checkBoxes (trn, mvs, bxs, sz) (Move ((x, y), Rght)) =
  let upperMoves = [Move ((x, y-1), Rght), Move ((x, y-1), Down), Move ((x+1, y-1), Down)]
      lowerMoves = [Move ((x, y+1), Rght), Move ((x, y), Down), Move ((x+1, y), Down)]
  in [Box (x, y) trn | lowerMoves `subset` mvs]
  ++ [Box (x, y-1) trn | upperMoves `subset` mvs]

checkBoxes (trn, mvs, bxs, sz) (Move ((x, y), Down)) =
  let leftMoves = [Move ((x-1, y), Rght), Move ((x-1, y), Down), Move ((x-1, y+1), Rght)]
      rightMoves = [Move ((x, y), Rght), Move ((x+1, y), Down), Move ((x, y+1), Rght)]
  in [Box (x-1, y) trn | leftMoves `subset` mvs]
  ++ [Box (x, y) trn | rightMoves `subset` mvs]

makeMove :: GameState -> Move -> Maybe GameState
makeMove game@(trn, mvs, bxs, sz) move@(Move ((x, y), Rght)) =
    if checkLegal game move
    then let newBoxes = checkBoxes game move
             next = if null newBoxes then turnSwap trn else trn
         in Just (next, mvs ++ [move], bxs ++ newBoxes, sz)
    else Nothing

makeMove game@(trn, mvs, bxs, sz) move@(Move ((x, y), Down)) =
    if checkLegal game move
    then let newBoxes = checkBoxes game move
             next = if null newBoxes then turnSwap trn else trn
         in Just (next, mvs ++ [move], bxs ++ newBoxes, sz)
    else Nothing

checkWinner :: GameState -> Maybe Winner
checkWinner game@(trn, mvs, bxs, (rows, cols)) = if length bxs >= rows * cols
                                       then Just $ calculateScore game
                                       else Nothing

calculateScore :: GameState -> Winner
calculateScore (trn, mvs, bxs, sz)
  | p1Score > p2Score = Winner PlayerOne
  | p1Score < p2Score = Winner PlayerTwo
  | otherwise         = Draw
  where (p1Boxes, p2Boxes) = partition (\(Box point player) -> player == PlayerOne) bxs
        p1Score = length p1Boxes
        p2Score = length p2Boxes

checkValidGame :: GameState -> Maybe GameState
checkValidGame game@(trn, mvs, bxs, (rows, cols)) = 
  let correctGame = playMoves (startGame rows cols) mvs
  in if correctGame == Just game
     then Just game
     else Nothing

playMoves :: GameState -> [Move] -> Maybe GameState
playMoves g [] = Just g
playMoves g (x:xs) = let maybeG = makeMove g x
                     in case maybeG of
                        Nothing -> Nothing
                        Just gm -> playMoves gm xs