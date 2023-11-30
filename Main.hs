module Main where
import DotsAndBoxes
import ReadAndShow (readGame, showGame, printMove, prettyShow, readMove, readUserMove, showMove)
import Solver
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.List.Extra (splitOn)
import System.Console.GetOpt
import Text.Read (readMaybe)

data Flag = Win | Depth String | Help | Verbose | Interactive | Mv String deriving (Show, Eq)
options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg Win) "Will fully run through the game and print the best move."
          , Option ['h'] ["help"] (NoArg Help) "Will give you a help menu."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Will allow you to change the depth of the searching to <num>, default value is 4."
          , Option ['m'] ["move"]  (ReqArg Mv "<move>") "Will place move <move> and print out the new board state."
          , Option ['v'] ["verbose"] (NoArg Verbose) "Outputs the move and a description of how good it is (win, lose, tie)"
          , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game against the computer"
          ]

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


main :: IO ()
main = do args <- getArgs
          let (flags, inputs, errors) = getOpt Permute options args
          if Help `elem` flags || not (null errors) 
          then putStrLn $ usageInfo "./DotsAndBoxes [options] [filename]\nOptions:" options
          else do let fname = if null inputs then "tests/two_from_end.txt" else head inputs 
                  contents <- readFile fname
                  let depth = findDepthFlag flags
                  let move = findMoveFlag flags
                  let game = readGame contents
                  case game of 
                    Nothing -> error "Invalid game file!"
                    Just gm -> if Win `elem` flags then putBestMove gm
                               else case move of
                                      Just mv -> do case makeMove gm mv of
                                                       Nothing -> error "Invalid move!"
                                                       Just newGame -> if Verbose `elem` flags 
                                                                       then prettyShow newGame
                                                                       else putStrLn $ showGame newGame
                                      Nothing -> putStrLn "Unfinished."
                                   --    Nothing -> do let (rating, goodMove) = whoMightWin gm depth
                                   --                  case goodMove of 
                                   --                     Nothing -> putStrLn "The game is finished."
                                   --                     Just gmv -> putStrLn $ showMove gmv




findDepthFlag :: [Flag] -> Int
findDepthFlag [] = 4
findDepthFlag (Depth d:xs) = read d :: Int
findDepthFlag (x:xs) = findDepthFlag xs

findMoveFlag :: [Flag] -> Maybe Move 
findMoveFlag [] = Nothing
findMoveFlag (Mv m:xs) = readUserMove m

findMoveFlag (x:xs) = findMoveFlag xs