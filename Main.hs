module Main where
import DotsAndBoxes
import ReadAndShow (readGame, showGame, printMove, prettyShow, readMove, readUserMove, showMove)
import Solver
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.List.Extra (splitOn)
import System.Console.GetOpt
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)

data Flag = Win | Depth String | Help | Verbose | Interactive | Mv String deriving (Show, Eq)
options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg Win) "Will fully run through the game and print the best move."
          , Option ['h'] ["help"] (NoArg Help) "Will give you a help menu."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Will allow you to change the depth of the searching to <num>, default value is 4."
          , Option ['m'] ["move"]  (ReqArg Mv "<move>") "Will place move <move> and print out the new board state.\n Move Format: \"(x,y,dir)\"\nx/y: integer\ndir: one of 'u', 'd', 'l', 'r'"
          , Option ['v'] ["verbose"] (NoArg Verbose) "Combine with -m to show the board and output a description of how good the move is."
          , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game against the computer."
          ]

printAllGames :: [Maybe GameState] -> Int -> IO ()
printAllGames [] n = do putStrLn "Good luck!"
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

makeMoveIO :: GameState -> Move -> Bool -> IO ()
makeMoveIO game move verbose = 
     case makeMove game move of
     Nothing -> putStrLn "That move is invalid! Try again."
     Just newGame -> if verbose
                     then do prettyShow newGame
                             case checkWinner newGame of
                                  Nothing -> putStrLn $ "This move results in a game rating of " ++ show (rateGame newGame) ++ "."
                                  Just (Winner PlayerOne) -> putStrLn "This move results in player one winning."
                                  Just (Winner PlayerTwo) -> putStrLn "This move results in player two winning."
                                  Just Draw -> putStrLn "This move results in a draw."
                     else putStrLn $ showGame newGame


main :: IO ()
main = do args <- getArgs
          let (flags, inputs, errors) = getOpt Permute options args
          if Help `elem` flags || not (null errors) 
          then putStrLn $ usageInfo "./DotsAndBoxes [options] [filename]\nOptions:" options
          else do let fname = if null inputs then "tests/two_from_end.txt" else head inputs -- safe use of head!
                  contents <- readFile fname
                  let depth = findDepthFlag flags
                  let move = findMoveFlag flags
                  let isVerbose = Verbose `elem` flags
                  case readGame contents of 
                    Nothing -> error "Invalid game file!"
                    Just game -> let action | Win `elem` flags = putBestMove game
                                            | not $ null move = case move of 
                                                                     [Just mv] -> makeMoveIO game mv isVerbose    
                                                                     _ -> putStrLn "Cannot read move!\nSee help (-h or --help) for move format."                    
                                            | Interactive `elem` flags = putStrLn "Work in progess!"
                                            | otherwise = case goodMove of
                                                               Nothing -> putStrLn "There is no move to make."
                                                               Just mv -> putStrLn ("A good move to make is " ++ printMove mv)
                                                          where (rating, goodMove) = whoMightWin game depth
                                 in action

findDepthFlag :: [Flag] -> Int
findDepthFlag [] = 4 -- default depth
findDepthFlag (Depth d:xs) 
     | depth < 0 = 0
     | otherwise = depth
     where depth = read d :: Int
findDepthFlag (x:xs) = findDepthFlag xs

findMoveFlag :: [Flag] -> [Maybe Move] 
findMoveFlag [] = []
findMoveFlag (Mv m:xs) = [readUserMove m]
findMoveFlag (x:xs) = findMoveFlag xs