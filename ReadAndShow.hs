module ReadAndShow where
import DotsAndBoxes
import Data.List.Extra (splitOn, intercalate)
import Text.Read (readMaybe)

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

readPlayer :: String -> Maybe Player
readPlayer str = case str of 
                 "P1" -> Just PlayerOne
                 "P2" -> Just PlayerTwo
                 _    -> Nothing

readMoves :: String -> Maybe [Move]
readMoves str = mapM readMove (splitOn "," str) 

readBoxes :: String -> Maybe [Box]
readBoxes str = mapM readBox (splitOn "," str)

-- ASK FOGARTY!!!!        
-- gives "no parse" error if xstr or ystr cannot be read, appropriate exception?? 
readMove :: String -> Maybe Move
readMove [xchar, ychar, dchar] = do x <- readMaybe [xchar] :: Maybe Int
                                    y <- readMaybe [ychar] :: Maybe Int
                                    dir <- readDir dchar
                                    Just $ Move ((x, y), dir)
                                 where readDir 'D' = Just Down
                                       readDir 'R' = Just Rght
                                       readDir  _  = Nothing
readMove str = Nothing

readBox :: String -> Maybe Box
readBox [xchar, ychar, pchar] = do x <- readMaybe [xchar] :: Maybe Int
                                   y <- readMaybe [ychar] :: Maybe Int
                                   p <- readTrn pchar
                                   Just $ Box (x, y) p
                                where readTrn '1' = Just PlayerOne
                                      readTrn '2' = Just PlayerTwo
                                      readTrn  _  = Nothing
readBox str = Nothing

showGame :: GameState -> String 
showGame (trn, mvs, bxs) = trnStr ++ "|" ++ mvsStr ++ "|" ++ bxsStr
                           where trnStr = showPlayer trn
                                 mvsStr = intercalate "," $ map showMove mvs
                                 bxsStr = intercalate "," $ map showBox bxs

showPlayer :: Player -> String
showPlayer p = case p of
               PlayerOne -> "P1"
               PlayerTwo -> "P2"

showMove :: Move -> String
showMove (Move ((x, y), dir)) = let xstr = show x
                                    ystr = show y
                                    dstr = case dir of
                                           Down -> "D"
                                           Rght -> "R"
                                in xstr ++ ystr ++ dstr

showBox :: Box -> String
showBox (Box (x, y) p) = let xstr = show x
                             ystr = show y
                             pstr = case p of
                                    PlayerOne -> "1"
                                    PlayerTwo -> "2"
                         in xstr ++ ystr ++ pstr

showDirection :: Direction -> String
showDirection Rght = "right"
showDirection Down = "down"

showWinner :: Winner -> String
showWinner Draw = "draw"
showWinner (Winner PlayerOne) = "Player one"
showWinner (Winner PlayerTwo) = "Player two"

printMove :: Move -> String
printMove (Move ((x, y), dir)) = "(" ++ show x ++ ", " ++ show y ++ ")" ++ " in the direction " ++ showDirection dir

prettyShow :: GameState -> IO ()
prettyShow game = let lns = printGameBoard game 
                  in mapM_ putStrLn lns

printGameBoard :: GameState -> [String]
printGameBoard gs@(trn,mvs,bxs) = 
  ("Turn: " ++ show trn) : [ if even y then printHorizontalLine gs (y `div` 2)
                             else printVerticalLine gs (y `div` 2) | y <- [0..(rows * 2)]]

printHorizontalLine :: GameState -> Int -> String
printHorizontalLine gs@(trn, mvs, bxs) y = 
  concat [ horizontalString gs (x, y) | x <- [0..columns]]

printVerticalLine :: GameState -> Int -> String
printVerticalLine gs@(trn, mvs, bxs) y = 
  concat [ verticalString gs (x, y) | x <- [0..columns]]

horizontalString :: GameState -> Point -> String
horizontalString (trn, mvs, bxs) (x, y) = if Move ((x, y), Rght) `elem` mvs then ".-" else ". "

verticalString :: GameState -> Point -> String
verticalString (trn, mvs, bxs) (x, y) = if Move ((x, y), Down) `elem` mvs
                                        then if Box (x, y) PlayerOne `elem` bxs then "|" ++ "1"
                                             else if Box (x, y) PlayerTwo `elem` bxs then "|" ++ "2"
                                             else "| "
                                        else "  "
