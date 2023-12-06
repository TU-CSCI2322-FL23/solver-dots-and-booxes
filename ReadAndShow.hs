module ReadAndShow where
import DotsAndBoxes
import Data.List.Extra (splitOn, intercalate)
import Text.Read (readMaybe)

readGame :: String -> Maybe GameState
readGame str = case splitOn "|" str of
               [szStr, trnStr, [], []]         -> do sz <- readSize szStr
                                                     trn <- readPlayer trnStr
                                                     checkValidGame (trn, [], [], sz)
               [szStr, trnStr, mvsStr, []]     -> do sz <- readSize szStr
                                                     trn <- readPlayer trnStr
                                                     mvs <- readMoves mvsStr
                                                     checkValidGame (trn, mvs, [], sz)
               [szStr, trnStr, mvsStr, bxsStr] -> do sz <- readSize szStr
                                                     trn <- readPlayer trnStr
                                                     mvs <- readMoves mvsStr
                                                     bxs <- readBoxes bxsStr
                                                     checkValidGame (trn, mvs, bxs, sz)
               _                               -> Nothing

readSize :: String -> Maybe (Int, Int)
readSize str = case splitOn "," str of 
               [rchar, cchar] -> do rows <- readMaybe rchar :: Maybe Int
                                    cols <- readMaybe cchar :: Maybe Int
                                    Just (rows, cols)
               _ -> Nothing

readPlayer :: String -> Maybe Player
readPlayer str = case str of 
                 "P1" -> Just PlayerOne
                 "P2" -> Just PlayerTwo
                 _    -> Nothing

readMoves :: String -> Maybe [Move]
readMoves str = mapM readMove (splitOn "/" str) 

readBoxes :: String -> Maybe [Box]
readBoxes str = mapM readBox (splitOn "/" str)

readMove :: String -> Maybe Move
readMove str = case splitOn "," str of
               [xchar, ychar, dchar] -> do x <- readMaybe xchar :: Maybe Int
                                           y <- readMaybe ychar :: Maybe Int
                                           dir <- readDir dchar
                                           Just $ Move ((x, y), dir)
                                        where readDir "D" = Just Down
                                              readDir "R" = Just Rght
                                              readDir  _  = Nothing
               _ -> Nothing

readUserMove :: String -> Maybe Move
readUserMove str
    = case splitOn "," str of 
      [xchar, ychar, dchar] -> do x <- readMaybe xchar :: Maybe Int
                                  y <- readMaybe ychar :: Maybe Int
                                  case dchar of 
                                    "d" -> Just $ Move ((x-1, y-1), Down)
                                    "r" -> Just $ Move ((x-1, y-1), Rght)
                                    "u" -> Just $ Move ((x-1, y-2), Down)
                                    "l" -> Just $ Move ((x-2, y-1), Rght)
                                    _   -> Nothing
      _ -> Nothing

readBox :: String -> Maybe Box
readBox str = case splitOn "," str of 
              [xchar, ychar, pchar] -> do x <- readMaybe xchar :: Maybe Int
                                          y <- readMaybe ychar :: Maybe Int
                                          p <- readTrn pchar
                                          Just $ Box (x, y) p
                                       where readTrn "1" = Just PlayerOne
                                             readTrn "2" = Just PlayerTwo
                                             readTrn  _  = Nothing
              _ -> Nothing

showGame :: GameState -> String 
showGame (trn, mvs, bxs, sz) = szStr ++ "|" ++ trnStr ++ "|" ++ mvsStr ++ "|" ++ bxsStr
                               where szStr = showSize sz
                                     trnStr = showPlayer trn
                                     mvsStr = intercalate "/" $ map showMove mvs
                                     bxsStr = intercalate "/" $ map showBox bxs

showSize :: (Int, Int) -> String
showSize (rows, columns) = intercalate "," [show rows, show columns]

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
                                in intercalate "," [xstr, ystr, dstr]

showBox :: Box -> String
showBox (Box (x, y) p) = let xstr = show x
                             ystr = show y
                             pstr = case p of
                                    PlayerOne -> "1"
                                    PlayerTwo -> "2"
                         in intercalate "," [xstr, ystr, pstr]

showDirection :: Direction -> String
showDirection Rght = "right"
showDirection Down = "down"

showWinner :: Winner -> String
showWinner Draw = "draw"
showWinner (Winner PlayerOne) = "Player one"
showWinner (Winner PlayerTwo) = "Player two"

printMove :: Move -> String
printMove (Move ((x, y), dir)) = "(" ++ show (x+1) ++ ", " ++ show (y+1) ++ ")" ++ " in the direction " ++ showDirection dir

prettyShow :: GameState -> IO ()
prettyShow game = let lns = printGameBoard game 
                  in mapM_ putStrLn lns

printGameBoard :: GameState -> [String]
printGameBoard gs@(trn,mvs,bxs, (rows, cols)) = 
  ("Turn: " ++ show trn) : [ if even y then printHorizontalLine gs (y `div` 2)
                             else printVerticalLine gs (y `div` 2) | y <- [0..(rows * 2)]]

printHorizontalLine :: GameState -> Int -> String
printHorizontalLine gs@(trn, mvs, bxs, (rows, cols)) y = 
  concat [ horizontalString gs (x, y) | x <- [0..cols]]

printVerticalLine :: GameState -> Int -> String
printVerticalLine gs@(trn, mvs, bxs, (rows, cols)) y = 
  concat [ verticalString gs (x, y) | x <- [0..cols]]

horizontalString :: GameState -> Point -> String
horizontalString (trn, mvs, bxs, sz) (x, y) = if Move ((x, y), Rght) `elem` mvs then ".-" else ". "

verticalString :: GameState -> Point -> String
verticalString (trn, mvs, bxs, sz) (x, y) = if Move ((x, y), Down) `elem` mvs
                                            then if Box (x, y) PlayerOne `elem` bxs then "|" ++ "1"
                                                 else if Box (x, y) PlayerTwo `elem` bxs then "|" ++ "2"
                                                 else "| "
                                            else "  "
