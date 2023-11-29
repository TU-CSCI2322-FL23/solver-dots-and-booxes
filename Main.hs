module Main where
import DotsAndBoxes
import ReadAndShow (readGame, showGame, printMove, prettyShow)
import Solver
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.List.Extra (splitOn)
import System.Console.GetOpt

data Flag = Win | Depth String | Help deriving (Show, Eq)
options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg Win) "Will fully run through the game and print the best move."
          , Option ['h'] ["help"] (NoArg Help) "Will give you a help menu."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Will allow you to change the depth of the searching to <num>, default value is 4."
          ]

depth = 4 :: Int
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
          if Help `elem` flags || not (null errors) then putStrLn $ usageInfo "Dots And Boxes [options] [filename]\nDots and Boxes project." options
          else do 
                    let fname = if null inputs then "tests/two_from_end.txt" else head inputs
                    let depth = findDepthFlag flags
                    contents <- readFile fname
                    let listOfGames = map readGame $ splitOn "\n" contents
                    if Win `elem` flags then printAllGames listOfGames 1 
          else do
                    putStrLn "PUT OTHER FLAGS HERE!!!!!"

findDepthFlag :: [Flag] -> Int
findDepthFlag [] = 4
findDepthFlag (Depth d:xs) = read d :: Int
findDepthFlag (x:xs) = findDepthFlag xs

findAllBestMoves :: [GameState] -> IO ()