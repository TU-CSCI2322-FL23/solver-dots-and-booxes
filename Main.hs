import DotsAndBoxes
import Data.Ratio ((%), Ratio)
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe ( catMaybes, isJust, fromMaybe, fromJust, isNothing )
import Debug.Trace
import Text.XHtml (base)
import Data.Char (ord)
import System.IO
import System.Environment
import System.Console.GetOpt
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

-- Can just be map (fromJust . readGame)
-- readAllGames :: [String] -> [GameState]
-- readAllGames [] = []
-- readAllGames (x:xs) = (fromJust (readGame x)):readAllGames xs

turnToGames :: String -> [GameState]
turnToGames str = let tmpstr = splitOn "\n" str
                  in map (fromJust . readGame) tmpstr

printAllGames :: [GameState] -> IO ()
printAllGames [] = putStrLn "Good luck on your games!"
printAllGames (x:xs) = do putStrLn "The current game looks like this: "
                          prettyShow x
                          putStr "The player I think will win is: "
                          hFlush stdout
                          putStrLn (printWinner (whoWillWin x))
                          putStr "The best move for the current player is: "
                          hFlush stdout 
                          putStrLn (printMove (bestMove x))
                          printAllGames xs

prompt :: String -> IO String
prompt question = 
  do putStrLn question
     hFlush stdout
     answer <- getLine
     return answer 

main :: IO ()
main = do args <- getArgs
          let fname = if null args then "tests/basic_tests.txt" else head args
          contents <- readFile (fname)
          let listOfGames = turnToGames contents
          printAllGames listOfGames

writeGame :: GameState -> FilePath -> IO ()
writeGame game file = writeFile file (showGame game)

loadGame :: FilePath -> IO GameState
loadGame file = do gameStr <- readFile file
                   let game = readGame gameStr
                   case game of 
                    Nothing -> error "Invalid game file!"
                    _       -> return $ fromJust game

putBestMove :: GameState -> IO ()
putBestMove game = do let move = bestMove game
                      case move of
                       Nothing -> putStrLn "There is no best move, you will lose :("
                       _       -> do putStrLn ("The best move for the current player is: " ++ printMove move)
                                     putStrLn "If the move is played, the game will look like this: "
                                     prettyShow $ fromJust $ makeMove game (fromJust move)
                                     case checkWinner $ fromJust $ makeMove game (fromJust move) of
                                        Just (Winner PlayerOne) -> putStrLn "Player one wins!!"
                                        Just (Winner PlayerTwo) -> putStrLn "Player two wins!!"
                                        Nothing -> putStrLn "Keep playing!!"
            
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
stringToBox [xstr, ystr, pstr] = if pstr `elem` ['1', '2']
                                 then let x = read [xstr] :: Int
                                          y = read [ystr] :: Int
                                          p = case pstr of
                                              '1' -> PlayerOne
                                              '2' -> PlayerTwo
                                      in Just $ Box (x,y) p
                                 else Nothing
stringToBox lst = Nothing

readPlayer :: String -> Maybe Player
readPlayer s = case s of 
               "P1" -> Just PlayerOne
               "P2" -> Just PlayerTwo
               _    -> Nothing

readMoves :: String -> Maybe [Move]
readMoves str = mapM stringToMove $ reverse (splitOn "," str) 

readBoxes :: String -> Maybe [Box]
readBoxes str = mapM stringToBox $ reverse (splitOn "," str)

readGame :: String -> Maybe GameState
readGame str = case splitOn "|" str of
               [trnStr, [], []]             -> do trn <- readPlayer trnStr
                                                  Just (trn, [], [])
               [trnStr, mvsStr, []]         -> do trn <- readPlayer trnStr
                                                  mvs <- readMoves mvsStr
                                                  Just (trn, mvs, [])
               [trnStr, mvsStr, bxsStr]     -> do trn <- readPlayer trnStr
                                                  mvs <- readMoves mvsStr
                                                  bxs <- readBoxes bxsStr
                                                  Just (trn, mvs, bxs)
               _                            -> Nothing

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
                            in xstr ++ ystr ++ pstr

showGame :: GameState -> String 
showGame (trn, mvs, bxs) = trnStr ++ "|" ++ mvsStr ++ "|" ++ bxsStr
                           where trnStr = turnToString trn
                                 mvsStr = intercalate "," $ map moveToString mvs
                                 bxsStr = intercalate "," $ map boxToString bxs

printMove :: Maybe Move -> String
printMove Nothing = "There isn't a move"
printMove (Just (Move ((x,y),dir))) = "(" ++ (show x) ++ "," ++ (show y) ++ ")" ++ " in the direction " ++ (showDir dir)

showDir :: Direction -> String
showDir Rght = "right"
showDir Down = "down"

printWinner :: Winner -> String
printWinner Draw = "draw"
printWinner (Winner PlayerOne) = "Player One"
printWinner (Winner PlayerTwo) = "Player Two"

-- is this acceptable? idk
checkValidGame :: GameState -> Bool
checkValidGame game@(trn, mvs, bxs) = let aux g [] = Just g
                                          aux g (x:xs) = let maybeG = makeMove g x
                                                         in case maybeG of
                                                            Nothing -> Nothing
                                                            _       -> aux (fromJust maybeG) xs
                                      in aux initGame (reverse mvs) == Just game

playMoves :: GameState -> [Move] -> Maybe GameState
playMoves g [] = Just g
playMoves g (x:xs) = let maybeG = makeMove g x
                     in case maybeG of
                        Nothing -> Nothing
                        _       -> playMoves (fromJust maybeG) xs