module Main where
import DotsAndBoxes
import Solver
import ReadAndShow (readGame, showGame, printMove, prettyShow, readMove, readUserMove, showMove)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.List.Extra (splitOn)
import System.Console.GetOpt

data Flag = Win | Depth String | Help | Verbose | Interactive | Mv String deriving (Show, Eq)
options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg Win) "Will fully run through the game and print the best move."
          , Option ['h'] ["help"] (NoArg Help) "Will give you a help menu."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Will allow you to change the depth of the searching to <num>, default value is 3."
          , Option ['m'] ["move"]  (ReqArg Mv "<move>") "Will place move <move> and print out the new board state.\nMove Format: \"x,y,dir\"\nx/y: integer\ndir: one of 'u', 'd', 'l', 'r'"
          , Option ['v'] ["verbose"] (NoArg Verbose) "Combine with -m to show the board and output a description of how good the move is."
          , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game against the computer."
          ]
          
main :: IO ()
main = do args <- getArgs
          let (flags, inputs, errors) = getOpt Permute options args
          if Help `elem` flags || not (null errors) 
          then putStrLn $ usageInfo "./DotsAndBoxes [options] [filename]\nOptions:" options
          else do let fname = if null inputs then "gamefiles/empty4x5.txt" else head inputs -- safe use of head!
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
                                               | Interactive `elem` flags = do prettyShow game
                                                                               playComputer game depth
                                               | otherwise = case goodMove of
                                                                  Nothing -> putStrLn "There is no move to make."
                                                                  Just mv -> putStrLn ("A good move to make is " ++ printMove mv)
                                                             where (rating, goodMove) = whoMightWin game depth
                                    in action


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
     
playComputer :: GameState -> Int -> IO ()
playComputer game@(PlayerOne, _, _, _) depth = 
  case checkWinner game of
  Just (Winner PlayerOne) -> putStrLn "You win!"
  Just (Winner PlayerTwo) -> putStrLn "You lose..."
  Just Draw -> putStrLn "You have drawn!"
  Nothing -> do moveStr <- prompt "Make a move: "
                case readUserMove moveStr of
                     Nothing   -> do putStrLn "Cannot read move!\nSee help (-h or --help) for move format. Try again." 
                                     playComputer game depth
                     Just move -> case makeMove game move of
                                       Nothing      -> do putStrLn "That move is invalid! Try again."
                                                          playComputer game depth
                                       Just newGame -> do prettyShow newGame
                                                          playComputer newGame depth

playComputer game@(PlayerTwo, _, _, _) depth = 
  case checkWinner game of
  Just (Winner PlayerOne) -> putStrLn "You win!"
  Just (Winner PlayerTwo) -> putStrLn "You lose..."
  Just Draw -> putStrLn "You have drawn!"
  Nothing -> do let (rating, move) = whoMightWin game depth
                case move of
                     Nothing -> putStrLn "Game is over!"
                     Just mv -> do putStrLn ("The computer makes the move " ++ printMove mv)
                                   case makeMove game mv of
                                        Nothing      -> do putStrLn "This should never happen.." -- whoMightWin only generates valid moves
                                        Just newGame -> do prettyShow newGame
                                                           playComputer newGame depth

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