import DotsAndBoxes
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          let fname = if null args then "tests/two_from_end.txt" else head args
          contents <- readFile (fname)
          let listOfGames = map readGame $ splitOn "\n" contents
          putStrLn ""
          printAllGames listOfGames

prompt :: String -> IO String
prompt question = 
  do putStrLn question
     hFlush stdout
     answer <- getLine
     return answer 

prettyShow :: GameState -> IO ()
prettyShow game = let lns = printGameBoard game
                  in mapM_ putStrLn lns

printAllGames :: [Maybe GameState] -> IO ()
printAllGames [] = do putStrLn "Good luck!"
                      putStrLn ""
printAllGames (x:xs) = case x of
                        Nothing   -> do putStrLn "This game is invalid!"
                        Just game -> do putBestMove game
                                        printAllGames xs

writeGame :: GameState -> FilePath -> IO ()
writeGame game file = writeFile file (showGame game)

loadGame :: FilePath -> IO (Maybe GameState)
loadGame file = do gameStr <- readFile file
                   let game = readGame gameStr
                   case game of 
                    Nothing -> return Nothing
                    Just gm -> return (Just gm)

putBestMove :: GameState -> IO ()
putBestMove game = do let move = bestMove game
                      case move of
                       Nothing -> do putStrLn "Game is over!"
                                     prettyShow game
                       Just mv -> do putStrLn ("The best move for the current player is: " ++ printMove mv)
                                     putStrLn "If the move is played, the game will look like this: "
                                     let newGame = makeMove game mv
                                     case newGame of
                                       Nothing -> error "This should not ever happen..."
                                       Just gm -> do prettyShow gm
                                                     case checkWinner gm of
                                                      Just (Winner PlayerOne) -> putStrLn "Player one wins!!"
                                                      Just (Winner PlayerTwo) -> putStrLn "Player two wins!!"
                                                      Nothing -> do putStrLn "Keep playing!!"
                      putStrLn ""

-- ASK FOGARTY!!!!        
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

-- ASK FOGARTY!!!!
-- gives "no parse" error if xstr or ystr cannot be read, appropriate exception?? 
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
readMoves str = mapM stringToMove (splitOn "," str) 

readBoxes :: String -> Maybe [Box]
readBoxes str = mapM stringToBox (splitOn "," str)

readGame :: String -> Maybe GameState
readGame str = case splitOn "|" str of
               [trnStr, [], []]             -> do trn <- readPlayer trnStr
                                                  checkValidGame (trn, [], [])
               [trnStr, mvsStr, []]         -> do trn <- readPlayer trnStr
                                                  mvs <- readMoves mvsStr
                                                  checkValidGame (trn, mvs, [])
               [trnStr, mvsStr, bxsStr]     -> do trn <- readPlayer trnStr
                                                  mvs <- readMoves mvsStr
                                                  bxs <- readBoxes bxsStr
                                                  checkValidGame (trn, mvs, bxs)
               _                            -> Nothing

turnToString :: Player -> String
turnToString p = case p of
                 PlayerOne -> "P1"
                 PlayerTwo -> "P2"

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

printMove :: Move -> String
printMove (Move ((x,y),dir)) = "(" ++ (show x) ++ "," ++ (show y) ++ ")" ++ " in the direction " ++ (showDir dir)

showDir :: Direction -> String
showDir Rght = "right"
showDir Down = "down"

printWinner :: Winner -> String
printWinner Draw = "draw"
printWinner (Winner PlayerOne) = "Player One"
printWinner (Winner PlayerTwo) = "Player Two"

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