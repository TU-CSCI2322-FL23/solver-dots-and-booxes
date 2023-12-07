module Solver where
import DotsAndBoxes
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List (partition, delete)

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


findBestRating :: GameState -> [(Rating,Move)] -> (Rating,Move) -> (Rating,Maybe Move)
findBestRating gs [] (rat,mv) = (rat, Just mv)
findBestRating gs@(PlayerOne, _, _, (rows, cols)) ((rat,mv):xs) (bestRat,bestMv)
  | rat == (2 * rows * cols) = (rat,Just mv)
  | rat > bestRat = findBestRating gs xs (rat,mv)
  | otherwise = findBestRating gs xs (bestRat,bestMv)
findBestRating gs@(PlayerTwo, _, _, (rows, cols)) ((rat,mv):xs) (bestRat,bestMv)
  | rat == ((-2) * rows * cols) = (rat, Just mv)
  | rat < bestRat = findBestRating gs xs (rat,mv)
  | otherwise = findBestRating gs xs (bestRat,bestMv)

getRatings :: [GameState] -> Int ->[Move] ->[Move]-> [Rating] -> [Rating]
getRatings [] depth [] made ans = ans
getRatings (x:xs) depth (y:ys) made ans = getRatings xs depth ys (y:made) (minimax x depth (ys++made):ans)

minimax :: GameState -> Int -> [Move] -> Rating
minimax game 0 mvs = rateGame game
minimax gs depth mvs = 
  case checkWinner gs of 
      Nothing -> fst $ findBestRating gs rating_mvs (head rating_mvs)
                  where gamestates = mapMaybe (makeMove gs) mvs
                        rating_mvs = zip (getRatings gamestates (depth-1) mvs [] []) mvs
      _ -> rateGame gs

whoMightWin :: GameState -> Int -> (Rating, Maybe Move)
whoMightWin gs 0 = (rateGame gs, Nothing)
whoMightWin gs depth = 
  case checkWinner gs of 
    Nothing -> let moves = findLegalMoves gs
                   gamestates = mapMaybe (makeMove gs) moves
                   rating_mvs = zip (getRatings gamestates (depth-1) moves [] []) moves
               in findBestRating gs rating_mvs (head rating_mvs)
    _ -> (rateGame gs, Nothing)

