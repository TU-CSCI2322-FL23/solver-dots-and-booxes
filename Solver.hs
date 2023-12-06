module Solver where
import DotsAndBoxes
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List (partition)
import Text.Libyaml (Style(Plain))
import Distribution.Simple.Utils (xargs)

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

findLargestRating :: GameState -> [(Rating, Maybe Move)] -> (Rating, Maybe Move)
findLargestRating gs [x] = x
findLargestRating gs@(PlayerOne, _, _, (rows, cols)) ((rating, mv):xs) 
  | rating == (2 * rows * cols) = (rating, mv)
  | rating > fst (findLargestRating gs xs) = (rating, mv)
  | otherwise = findLargestRating gs xs
findLargestRating gs@(PlayerTwo, _, _, (rows, cols)) ((rating, mv):xs) 
  | rating == ((-2) * rows * cols) = (rating, mv)
  | rating < fst (findLargestRating gs xs) = (rating, mv)
  | otherwise = findLargestRating gs xs

whoMightWin :: GameState -> Int -> (Rating, Maybe Move)
whoMightWin gs 0 = (rateGame gs, Nothing)
whoMightWin gs@(_, _, _, (rows, cols)) depth = 
  case checkWinner gs of 
    Nothing -> let moves = findLegalMoves gs
                   gamestates = mapMaybe (makeMove gs) moves
                   movesAndGames = zip moves gamestates
                   minimax :: GameState -> Move -> Int -> (Rating, Maybe Move)
                   minimax game move 0 = (rateGame game, Just move)
                   minimax gs@(_, _, _, (rows, cols)) move depth = 
                     case checkWinner gs of 
                          Nothing -> findLargestRating gs (map (\game -> minimax game move (depth-1)) gamestates)
                                     where gamestates = mapMaybe (makeMove gs) (findLegalMoves gs)
                          _ -> (rateGame gs, Nothing)
               in findLargestRating gs (map (\(move, game) -> minimax game move (depth-1)) movesAndGames)
    _ -> (rateGame gs, Nothing)

