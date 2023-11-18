module Display where
import DotsAndBoxes

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