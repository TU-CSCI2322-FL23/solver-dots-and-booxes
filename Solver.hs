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


findBestRating :: GameState -> [(Rating, Maybe Move)] -> (Rating , Maybe Move) -> (Rating, Maybe Move)
findBestRating gs [] best = best
findBestRating gs@(PlayerOne, _, _, (rows, cols)) ((rating, mv):xs) (bestRat,bestMv)
  | rating == (2 * rows * cols) = (rating, mv)
  | rating > bestRat = findBestRating gs xs (rating,mv)
  | otherwise = findBestRating gs xs (bestRat,bestMv)
findBestRating gs@(PlayerTwo, _, _, (rows, cols)) ((rating, mv):xs) (bestRat,bestMv)
  | rating == ((-2) * rows * cols) = (rating, mv)
  | rating < bestRat = (rating, mv)
  | otherwise = findBestRating gs xs (bestRat,bestMv)

whoMightWin :: GameState -> Int -> (Rating, Maybe Move)
whoMightWin gs 0 = (rateGame gs, Nothing)
whoMightWin gs depth = 
  case checkWinner gs of 
    Nothing -> let moves = findLegalMoves gs
                   gamestates = mapMaybe (makeMove gs) moves
                   movesAndGames = zip moves gamestates
                   rating_mvs :: [(Rating, Maybe Move)]
                   rating_mvs = map (\(move, game) -> minimax game move (depth-1) (delete move moves)) movesAndGames
                   minimax :: GameState -> Move -> Int -> [Move] -> (Rating, Maybe Move)
                   minimax game move 0 mvs = (rateGame game, Just move)
                   minimax gs move depth mvs = 
                     case checkWinner gs of 
                          Nothing -> findBestRating gs rat_mvs (head rat_mvs)
                                     where gamestates_mvs = zip (mapMaybe (makeMove gs) mvs) mvs
                                           rat_mvs = map (\(game,mv) -> minimax game move (depth-1) (delete mv mvs)) gamestates_mvs
                          _ -> (rateGame gs, Just move)
               in findBestRating gs rating_mvs (head rating_mvs)
    _ -> (rateGame gs, Nothing)

