import DotsAndBoxes
import Data.Ratio ((%), Ratio)
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe ( catMaybes, isJust, fromMaybe, fromJust )
import Debug.Trace
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
-- gives "no parse" error if xstr or ystr cannot be read, appropriate exception??
stringToMove :: String -> Maybe Move
stringToMove [xstr, ystr, dstr] = if dstr `elem` ['D', 'R']
                                  then let x = read [xstr] :: Int
                                           y = read [ystr] :: Int
                                           dir = case dstr of
                                                 'D' -> Down
                                                 'R' -> Rght
                                       in Just $ Move ((x, y), dir)
                                 else Nothing
stringToMove lst = Nothing

stringToBox :: String -> Maybe Box
stringToBox [xstr,ystr,pstr] = let x = read [xstr] :: Int
                                   y = read [ystr] :: Int
                                   p = case pstr of
                                        '1' -> PlayerOne
                                        '2' -> PlayerTwo
                                in Just $ Box (x,y) p
stringToBox lst = Nothing

-- plays moves on a gamestate and returns maybe gamestate if moves are valid
playMoves :: GameState -> [Move] -> Maybe GameState
playMoves game [] = Just game
playMoves game (mv:mvs) = let newGame = makeMove game mv
                          in case newGame of
                             Nothing -> Nothing
                             _ -> playMoves (fromJust newGame) mvs


readPlayer :: String -> Player
readPlayer s = if s == "P1" then PlayerOne else PlayerTwo

readMoves :: String -> [Move]
readMoves str = moveMaker (splitOn "," str)

moveMaker :: [String] -> [Move]
moveMaker [] = []
moveMaker (x:xs) = fromJust (stringToMove x) : moveMaker xs

readBoxes :: String -> [Box]
readBoxes str = boxMaker (splitOn "," str)

boxMaker :: [String] -> [Box]
boxMaker [] = []
boxMaker (x:xs) = fromJust (stringToBox x) : boxMaker xs

readGame :: String -> Maybe GameState
readGame str = 
        case lines str of
            [trn,mvs,bxs] -> Just (readPlayer trn, readMoves mvs, readBoxes bxs)
            _ -> Nothing


turnToString :: Player -> String
turnToString p = case p of
                 PlayerOne -> "P1"
                 PlayerTwo -> "P2"

-- turns move into move string, i.e. Move ((3, 3), Rght) -> "33R"
moveToString :: Move -> String
moveToString (Move ((x, y), dir)) = let xstr = show x
                                        ystr = show y
                                        dstr = case dir of
                                               Down -> "D"
                                               Rght -> "R"
                                    in xstr ++ ystr ++ dstr

boxToString :: Box -> String
boxToString (Box (x,y) p) = let xstr = show x
                                ystr = show y
                                pstr = case p of
                                        PlayerOne -> "1"
                                        PlayerTwo -> "2"
                            in xstr++ystr++pstr

-- turns gamestate into game string, i.e. (PlayerOne, [Move ((2,2),Down),Move ((0,0),Rght)], []) -> "00R,22D"
showGame :: Maybe GameState -> String 
showGame (Just (trn, mvs, bxs)) = trnStr++"\n"++mvsStr++"\n"++bxsStr
                            where trnStr = turnToString trn
                                  mvsStr = intercalate "," $ map moveToString mvs
                                  bxsStr = intercalate "," $ map boxToString bxs
showGame Nothing = ""