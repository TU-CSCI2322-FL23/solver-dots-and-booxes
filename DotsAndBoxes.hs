module DotsAndBoxes where
import Data.List (intercalate, partition)
import Data.Maybe (catMaybes, mapMaybe)

data Player = PlayerOne | PlayerTwo deriving (Eq, Show)

type Point = (Int, Int)
data Direction = Rght | Down deriving (Eq, Show)
type Line = (Point, Direction)
newtype Move = Move Line deriving (Eq, Show)

data Box = Box Point Player deriving (Eq, Show) -- top left point of box and who controls it

type GameState = (Player, [Move], [Box]) -- whose turn it is, list of moves done, list of boxes completed

data Winner = Winner Player | Draw deriving (Eq, Show)

-- initial gamestate
initGame = (PlayerOne, [], []) :: GameState

-- starting out 4x5 boxes
rows = 4 :: Int
columns = 5 :: Int
numBoxes = rows * columns :: Int

createAllMoves :: Int -> Int -> [Move]
createAllMoves m n = [ Move ((x, y), Rght) | x <- [0..n-1], y <- [0..m] ]
                  ++ [ Move ((x, y), Down) | x <- [0..n], y <- [0..m-1] ]

findLegalMoves :: GameState -> [Move]
findLegalMoves (trn, mvs, bxs) = let allMoves = createAllMoves rows columns
                                 in [ move | move <- allMoves, move `notElem` mvs ]

checkBounds :: Move -> Bool
checkBounds (Move ((x, y), Rght)) = x >= 0 && x < columns && y >= 0 && y <= rows
checkBounds (Move ((x, y), Down)) = x >= 0 && x <= columns && y >= 0 && y < rows

checkLegal :: GameState -> Move -> Bool
checkLegal (trn, mvs, bxs) move = move `notElem` mvs && checkBounds move

turnSwap :: Player -> Player
turnSwap trn = if trn == PlayerOne then PlayerTwo else PlayerOne

subset :: Eq a => [a] -> [a] -> Bool
subset [] lst = True
subset (x:xs) lst
  | x `elem` lst = subset xs lst
  | otherwise    = False

checkBoxes :: GameState -> Move -> [Box]
checkBoxes (trn, mvs, bxs) (Move ((x, y), Rght)) = 
  let upperMoves = [Move ((x, y-1), Rght), Move ((x, y-1), Down), Move ((x+1, y-1), Down)]
      lowerMoves = [Move ((x, y+1), Rght), Move ((x, y), Down), Move ((x+1, y), Down)]
  in [Box (x, y) trn | lowerMoves `subset` mvs] 
  ++ [Box (x, y-1) trn | upperMoves `subset` mvs]

checkBoxes (trn, mvs, bxs) (Move ((x, y), Down)) = 
  let leftMoves = [Move ((x-1, y), Rght), Move ((x-1, y), Down), Move ((x-1, y+1), Rght)]
      rightMoves = [Move ((x, y), Rght), Move ((x+1, y), Down), Move ((x, y+1), Rght)]
  in [Box (x-1, y) trn | leftMoves `subset` mvs] 
  ++ [Box (x, y) trn | rightMoves `subset` mvs]

makeMove :: GameState -> Move -> Maybe GameState
makeMove game@(trn, mvs, bxs) move@(Move ((x, y), Rght)) =
    if checkLegal game move
    then let newBoxes = checkBoxes game move
             next = if null newBoxes then turnSwap trn else trn                                         
         in Just (next, mvs ++ [move], bxs ++ newBoxes)
    else Nothing

makeMove game@(trn, mvs, bxs) move@(Move ((x, y), Down)) =
    if checkLegal game move
    then let newBoxes = checkBoxes game move
             next = if null newBoxes then turnSwap trn else trn                                         
         in Just (next, mvs ++ [move], bxs ++ newBoxes)
    else Nothing

checkWinner :: GameState -> Maybe Winner
checkWinner game@(trn, mvs, bxs) = if length bxs >= numBoxes
                                   then Just $ calculateScore game
                                   else Nothing

calculateScore :: GameState -> Winner
calculateScore (trn, mvs, bxs) 
  | p1Score > p2Score = Winner PlayerOne
  | p1Score < p2Score = Winner PlayerTwo
  | otherwise         = Draw
  where (p1Boxes, p2Boxes) = partition (\(Box point player) -> player == PlayerOne) bxs
        p1Score = length p1Boxes
        p2Score = length p2Boxes

whoWillWin :: GameState -> Winner
whoWillWin gs@(trn, mvs, bxs) =
  let possibleGS = mapMaybe (makeMove gs) (findLegalMoves gs)
      aux :: [GameState] -> Bool -> Winner
      aux [] drawn = if drawn then Draw else Winner (turnSwap trn)
      aux (x:xs) drawn = case whoWillWin x of 
        Winner foo -> if foo == trn then Winner trn else aux xs drawn
        Draw -> aux xs True
  in case checkWinner gs of
     Just win -> win
     Nothing -> aux possibleGS False

bestMove :: GameState -> Maybe Move
bestMove gs@(trn, mvs, bxs) =
  let possibleMvs = findLegalMoves gs
      possibleGS = zip possibleMvs (map (makeMove gs) possibleMvs)
      aux :: [(Move, Maybe GameState)] -> Maybe Move -> Maybe Move
      aux [] (Just mv) = Just mv
      aux [] Nothing = if null possibleMvs then Nothing else Just (head possibleMvs)
      aux ((x, Nothing):xs) mv = aux xs mv
      aux ((x, Just xgs):xs) mv = case whoWillWin xgs of
                                  Winner foo -> if foo == trn then Just x else aux xs mv
                                  Draw -> aux xs (Just x)
  in aux possibleGS Nothing

-- ASK FOGARTY!!!!
-- Is this the correct way to implement this?? Needs FOGARTY'S APPROVAL!!!
checkValidGame :: GameState -> Maybe GameState
checkValidGame game@(trn, mvs, bxs) = let correctGame = playMoves initGame mvs
                                      in if correctGame == Just game
                                         then Just game
                                         else Nothing

playMoves :: GameState -> [Move] -> Maybe GameState
playMoves g [] = Just g
playMoves g (x:xs) = let maybeG = makeMove g x
                     in case maybeG of
                        Nothing -> Nothing
                        Just gm -> playMoves gm xs