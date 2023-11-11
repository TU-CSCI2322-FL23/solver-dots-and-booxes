import DotsAndBoxes
import Data.Ratio ((%), Ratio)
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe ( catMaybes, isJust, fromMaybe, fromJust )
import Debug.Trace ()
import Text.XHtml (base)
import Data.Char (ord)

{-
    Story 1 DONE
    Story 2 DONE
    Choudry and Aashish Story 3 whoWillWin :: Game -> Winner 
    All Story 4 bestMove :: Game -> Move
    Liam and August Story 5 simple text format
    Liam and August Story 6 readGame :: String -> Game "3 2 R, 1 2 D, 5 6 R...."
    Liam and August Story 7 showGame :: Game -> String
    Liam Story 8 one each to read and write game states from a file, one that computes and prints the winning move, and a simple main action. 
    Liam Story 9 test cases for each function
    August Story 10 consider possible errors or edge cases. Return a Maybe Move, Maybe Game,  etc.
-}


main :: IO ()
main = undefined

-- turns move string into maybe move, i.e. "33R" -> Move ((3, 3), Rght)
stringToMove :: String -> Maybe Move
stringToMove (xstr:ystr:dstr) = let x = read [xstr] :: Int
                                    y = read [ystr] :: Int
                                    dir = case dstr of
                                          "D" -> Down
                                          "R" -> Rght
                                in Just $ Move ((x, y), dir)
stringToMove _ = Nothing

-- plays moves on a gamestate and returns maybe gamestate if moves are valid
playMoves :: GameState -> [Move] -> Maybe GameState
playMoves game [] = Just game
playMoves game (mv:mvs) = let newGame = makeMove game mv
                          in case newGame of
                             Nothing -> Nothing
                             _ -> playMoves (fromJust newGame) mvs

-- turns game string into maybe gamestate, i.e. "00R,22D" -> (PlayerOne, [Move ((2,2),Down),Move ((0,0),Rght)], [])
readGame :: String -> Maybe GameState
readGame str = let moves = mapM stringToMove (splitOn "," str)              
               in case moves of
                  Nothing -> Nothing
                  _ -> playMoves initGame (fromJust moves)

-- turns move into move string, i.e. Move ((3, 3), Rght) -> "33R"
moveToString :: Move -> String
moveToString (Move ((x, y), dir)) = let xstr = show x
                                        ystr = show y
                                        dstr = case dir of
                                               Down -> "D"
                                               Rght -> "R"
                                    in xstr ++ ystr ++ dstr

-- turns gamestate into game string, i.e. (PlayerOne, [Move ((2,2),Down),Move ((0,0),Rght)], []) -> "00R,22D"
showGame :: GameState -> String 
showGame (trn, mvs, bxs) = intercalate "," $ map moveToString $ reverse mvs