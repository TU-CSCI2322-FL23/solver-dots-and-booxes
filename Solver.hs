module Solver where
import DotsAndBoxes
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List (partition)
import Text.Libyaml (Style(Plain))

type Rating = Int

whoWillWin :: GameState -> Winner
whoWillWin gs@(trn, _, _, _) =
  let possibleGS = mapMaybe (makeMove gs) (findLegalMoves gs)
      outcomes = map whoWillWin possibleGS
      bestOutcome
        | Winner trn `elem` outcomes = Winner trn
        | Draw `elem` outcomes       = Draw
        | otherwise                  = Winner (turnSwap trn)
  in fromMaybe bestOutcome (checkWinner gs)

bestMove :: GameState -> Maybe Move
bestMove gs@(trn, _, _, _) =
  let possibleMvs = findLegalMoves gs
      possibleGS = zip possibleMvs (map (makeMove gs) possibleMvs)
      aux :: [(Move, Maybe GameState)] -> Maybe Move -> Maybe Move
      aux [] (Just mv) = Just mv
      aux [] Nothing = if null possibleMvs then Nothing else Just (head possibleMvs)
      aux ((x, Nothing):xs) mv = aux xs mv
      aux ((x, Just xgs):xs) mv = case whoWillWin xgs of
                                       Winner foo -> if foo == trn then Just x else aux xs mv
                                       Draw -> aux xs (Just x)
  in aux possibleGS Nothing


rateGame :: GameState -> Rating
rateGame gs@(_, _, bxs, (rows, cols)) = case checkWinner gs of
                              Nothing -> let (p1Boxes, p2Boxes) = partition (\(Box point player) -> player == PlayerOne) bxs
                                             p1Score = length p1Boxes
                                             p2Score = length p2Boxes
                                         in p1Score - p2Score
                              Just (Winner PlayerOne) -> 2 * rows * cols
                              Just (Winner PlayerTwo) -> (-2) * rows * cols
                              Just Draw -> 0 

-- when rating_mvs is calculated the correct working banks on the fact that whenever a move from the list of legalmoves is made it wouldn't return a Nothing. If it returns nothing zipping the rating to moves wouldn't work because the length of ratings might be less then length of moves.
whoMightWin :: GameState -> Int -> (Rating, Maybe Move)
whoMightWin gs@(trn, _, _, (rows,cols)) depth = 
    let recur_depth :: Int -> GameState -> Int
        recur_depth 0 xgs = rateGame xgs
        recur_depth depth xgs@(xtrn,_,_,(xrows,xcols)) = case checkWinner xgs of
          Nothing -> let possibleGS =  mapMaybe (makeMove xgs) (findLegalMoves xgs)
                         ratings = map (recur_depth (depth-1)) possibleGS
                         iterator :: [Int] -> Int -> Int
                         iterator [] ans = ans
                         iterator (x:xs) ans
                          | xtrn == PlayerOne && x == 2 * product xsz = x
                          | xtrn == PlayerTwo && x == (-2) * product xsz = x
                          | xtrn == PlayerOne && x > ans = iterator xs x
                          | xtrn == PlayerTwo && x < ans = iterator xs x
                          | otherwise = iterator xs ans
                      in iterator ratings (head ratings)
          Just (Winner PlayerOne) -> 2 * product sz 
          Just (Winner PlayerTwo) -> (-2) * product sz
          Just Draw -> 0 

    in case checkWinner gs of
      Nothing -> 
        let possibleMvs = findLegalMoves gs
            rating_mvs :: [(Int, Move)]
            rating_mvs = zip (map (recur_depth depth) (mapMaybe (makeMove gs) possibleMvs)) possibleMvs
            format_var = out_iter rating_mvs (head rating_mvs)
            out_iter :: [(Int, Move)] -> (Int, Move) -> (Int, Move)
            out_iter [] ans = ans 
            out_iter (x:xs) ans 
              | trn == PlayerOne && fst x == 2 * product sz = x
              | trn == PlayerTwo && fst x == (-2) * product sz = x
              | trn == PlayerOne && x > ans = out_iter xs x
              | trn == PlayerTwo && x < ans = out_iter xs x
              | otherwise = out_iter xs ans
        in (fst format_var, Just (snd format_var))
      Just (Winner PlayerOne) -> (rateGame gs, Nothing)
      Just (Winner PlayerTwo) -> (rateGame gs, Nothing)
      Just Draw -> (rateGame gs, Nothing)

-- debug methods to make a sample game and keep running moves on it unit test sort of

temp :: Maybe GameState -> GameState
temp a = head (catMaybes [a])

simulateGame :: Int -> GameState -> GameState
simulateGame 0 gs = gs
simulateGame mvs gs = case checkWinner gs of
  Nothing -> simulateGame (mvs-1) (temp(makeMove gs (head $ findLegalMoves gs)))
  Just(Winner PlayerOne) -> gs
  Just(Winner PlayerTwo) -> gs
  Just Draw -> gs
-- Unit Tests
-- a = startGame 1 2
-- b = temp(makeMove a (head $ findLegalMoves a))
-- c = temp(makeMove b (head $ findLegalMoves b))
-- d = temp(makeMove c (head $ findLegalMoves c))
-- e = temp(makeMove d (head $ findLegalMoves d))