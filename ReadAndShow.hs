module ReadAndShow where
import DotsAndBoxes
import Data.List.Extra (splitOn, intercalate)

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
readMove [xstr, ystr, dstr] = if dstr `elem` ['D', 'R']
                                  then let x = read [xstr] :: Int
                                           y = read [ystr] :: Int
                                           dir = case dstr of
                                                 'D' -> Down
                                                 'R' -> Rght
                                       in Just $ Move ((x, y), dir)
                                 else Nothing
readMove str = Nothing

readBox :: String -> Maybe Box
readBox [xstr, ystr, pstr] = if pstr `elem` ['1', '2']
                                 then let x = read [xstr] :: Int
                                          y = read [ystr] :: Int
                                          p = case pstr of
                                              '1' -> PlayerOne
                                              '2' -> PlayerTwo
                                      in Just $ Box (x, y) p
                                 else Nothing
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
