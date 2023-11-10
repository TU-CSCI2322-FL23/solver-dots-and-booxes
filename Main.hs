import DotsAndBoxes
import Data.Ratio ((%), Ratio)
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.List.Split ()
import Data.Maybe ( catMaybes, isJust )
import Debug.Trace ()
import Text.XHtml (base)

{-
    Story 1 DONE
    Story 2 DONE
    Choudry and Aashish Story 3 whoWillWin :: Game -> Winner 
    All Story 4 bestMove :: Game -> Move
    Liam and August Story 5 simple text format
    Liam and August Story 6 readGame :: String -> Game "3 2 R, 1 2 D, 5 6 R...."
    Liam and August Story 7 showGame :: Game -> String
    Liam Story 8 one each to read and write game states from a file, one that computes and prints the winning move, and a simple main action. 
    Liam Story 9 test cases for each function
    August Story 10 consider possible errors or edge cases. Return a Maybe Move, Maybe Game,  etc.
-}


main :: IO ()
main = do
    