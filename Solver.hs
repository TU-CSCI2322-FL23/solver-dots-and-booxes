module Solver where
import DotsAndBoxes
import Data.Maybe (mapMaybe, fromMaybe)
import GHC.Real (infinity)
import Data.List (partition)

whoWillWin :: GameState -> Winner
whoWillWin gs@(trn, mvs, _) =
  let possibleGS = mapMaybe (makeMove gs) (findLegalMoves gs)
      outcomes = map whoWillWin possibleGS
      bestOutcome
        | Winner trn `elem` outcomes = Winner trn
        | Draw `elem` outcomes       = Draw
        | otherwise                  = Winner (turnSwap trn)
  in fromMaybe bestOutcome (checkWinner gs)

bestMove :: GameState -> Maybe Move
bestMove gs@(trn, mvs, _) =
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


-- Have changed the return type to Rational from Integer. If game dimensions in GS then it could be done with Int
rateGame :: GameState -> Rational
rateGame gs@(_,_,bxs) = case checkWinner gs of
                            Nothing -> let (p1Boxes, p2Boxes) = partition (\(Box point player) -> player == PlayerOne) bxs
                                           p1Score = length p1Boxes
                                           p2Score = length p2Boxes
                                        in toRational (p1Score - p2Score)
                            Just (Winner PlayerOne) -> infinity
                            Just (Winner PlayerTwo) -> -infinity
                        
