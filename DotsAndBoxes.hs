module DotsAndBoxes where
import Data.Ratio ((%), Ratio)
import Data.Tuple (swap)
import Data.List (intercalate, transpose)
import Data.List.Split ()
import Data.Maybe ( catMaybes, isJust, mapMaybe )
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

-- initial gamestate
initGame = (PlayerOne, [], []) :: GameState

--starting out 5x4 boxes
rows = 4 :: Int
columns = 5 :: Int
numBoxes = rows * columns :: Int

-- creates a list of all possible moves for an m x n board
createAllMoves :: Int -> Int -> [Move]
createAllMoves m n = [ Move ((x, y), Rght) | x <- [0..n-1], y <- [0..m] ]
                  ++ [ Move ((x, y), Down) | x <- [0..n], y <- [0..m-1] ]

-- finds all the possible legal moves for a gamestate
findLegalMoves :: GameState -> [Move]
findLegalMoves (trn, mvs, bxs) = let allMoves = createAllMoves rows columns
                                 in [ move | move <- allMoves, move `notElem` mvs ]

checkBounds :: Move -> Bool
checkBounds (Move ((x, y), Rght)) = x >= 0 && x < columns && y >= 0 && y <= rows
checkBounds (Move ((x, y), Down)) = x >= 0 && x <= columns && y >= 0 && y < rows

-- checks if a move is legal
checkLegal :: GameState -> Move -> Bool
checkLegal (trn, mvs, bxs) move = move `notElem` mvs && checkBounds move

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

-- updates gamestate with move
makeMove :: GameState -> Move -> Maybe GameState
makeMove game@(trn, mvs, bxs) move@(Move ((x, y), Rght)) =
    if checkLegal game move
    then let newBoxes = checkBoxes game move
             next = if null newBoxes then turnSwap trn else trn                                         
         in Just (next, move:mvs, newBoxes ++ bxs)
    else Nothing

makeMove game@(trn, mvs, bxs) move@(Move ((x, y), Down)) =
    if checkLegal game move
    then let newBoxes = checkBoxes game move
             next = if null newBoxes then turnSwap trn else trn                                         
         in Just (next, move:mvs, newBoxes ++ bxs)
    else Nothing

-- checks if there is a winner and returns winner if so
checkWinner :: GameState -> Maybe Winner
checkWinner game@(trn, mvs, bxs) = if length bxs >= numBoxes
                                   then Just (calculateScore game (0, 0))
                                   else Nothing

-- calculates the final score and returns a winner or a draw
calculateScore :: GameState -> (Int, Int) -> Winner
calculateScore (trn, mvs, []) (p1score, p2score)
  | p1score > p2score = Winner PlayerOne
  | p2score > p1score = Winner PlayerTwo
  | otherwise = Draw
calculateScore (trn, mvs, (Box _ player):bxs) (p1score, p2score) = 
  case player of
  PlayerOne -> calculateScore (trn, mvs, bxs) (p1score+1, p2score)
  PlayerTwo -> calculateScore (trn, mvs, bxs) (p1score, p2score+1)

--return horizontal line
printHorizontalLine :: GameState -> Int -> String
printHorizontalLine (trn, mvs, bxs) y = 
  concat [ if Move ((x, (y `div` 2)), Rght) `elem` mvs then ".-" else ". " | x <- [0..columns]]

--return Vertical line
printVerticalLine :: GameState -> Int -> String
printVerticalLine (trn, mvs, bxs) y = 
  let p = if trn == PlayerOne then "1" else "2"
  in concat [ if Move ((x, (y `div` 2)), Down) `elem` mvs
              then if Box (x, (y `div` 2)) PlayerOne `elem` bxs then "|" ++ "1"
                   else if Box (x, (y `div` 2)) PlayerTwo `elem` bxs then "|" ++ "2"
                   else "| "
              else "  " | x <- [0..columns]]

--return the game board
printGameBoard :: GameState -> [String]
--printGameBoard (trn, mvs, bxs) ((rows*2)+1) = if trn == PlayerOne then " Player One's Turn" else " Player Two's Turn"
printGameBoard (trn,mvs,bxs) = 
  ("Turn: " ++ show trn) : [ if even y then printHorizontalLine (trn, mvs, bxs) y
                             else printVerticalLine (trn, mvs, bxs) y | y <- [0..(rows*2)]]

prettyShow :: GameState -> IO ()
prettyShow game = let lns = printGameBoard game
                  in mapM_ putStrLn lns

turnSwap :: Player -> Player
turnSwap trn = if trn == PlayerOne then PlayerTwo else PlayerOne

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

-- might need to handle edge cases
bestMove :: GameState -> Maybe Move
bestMove gs@(trn, mvs, bxs) =
  let possibleMvs = findLegalMoves gs
      possibleGS = zip possibleMvs (map (makeMove gs) possibleMvs)
      aux :: [(Move,Maybe GameState)] ->Maybe Move -> Maybe Move
      aux [] (Just mv) = Just mv
      aux [] Nothing = Nothing
      aux ((x,Nothing):xs) mv = aux xs  mv
      aux ((x,Just xgs):xs) mv = case whoWillWin xgs of
        Winner foo -> if foo == trn then Just x else aux xs mv
        Draw -> aux xs (Just x)
  in aux possibleGS Nothing