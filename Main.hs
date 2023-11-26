module Main where
import DotsAndBoxes
import ReadAndShow (readGame, showGame, printMove, prettyShow)
import Solver
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.List.Extra (splitOn)

main :: IO ()
main = do args <- getArgs
          let fname = if null args then "tests/two_from_end.txt" else head args
          contents <- readFile fname
          let listOfGames = map readGame $ splitOn "\n" contents
          putStrLn ""
          printAllGames listOfGames 1

printAllGames :: [Maybe GameState] -> Int -> IO ()
printAllGames [] n = do putStrLn "Good luck!"
                        putStrLn ""
printAllGames (x:xs) n = do putStrLn ("Game " ++ show n ++ ":")
                            case x of
                                 Nothing -> do putStrLn "This game is invalid!"
                                               printAllGames xs (n+1)
                                 Just game -> do putBestMove game
                                                 printAllGames xs (n+1)

writeGame :: GameState -> FilePath -> IO ()
writeGame game file = writeFile file (showGame game)

loadGame :: FilePath -> IO (Maybe GameState)
loadGame file = do gameStr <- readFile file
                   case readGame gameStr of 
                        Nothing -> return Nothing
                        Just gm -> return (Just gm)

putBestMove :: GameState -> IO ()
putBestMove game = case bestMove game of
                        Nothing -> do putStrLn "Game is over!"
                                      prettyShow game
                        Just mv -> do putStrLn ("The best move for the current player is: " ++ printMove mv)
                                      putStrLn "If the move is played, the game will look like this: "
                                      case makeMove game mv of
                                           Nothing -> error "This should never happen..."
                                           Just gm -> do prettyShow gm
                                                         case checkWinner gm of
                                                              Just (Winner PlayerOne) -> putStrLn "Player one wins!"
                                                              Just (Winner PlayerTwo) -> putStrLn "Player two wins!"
                                                              Nothing -> do putStrLn "Keep playing!"

prompt :: String -> IO String
prompt question = do putStrLn question
                     hFlush stdout
                     getLine
