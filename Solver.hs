module Solver where
import DotsAndBoxes
import Data.Maybe (mapMaybe, fromMaybe)
import GHC.Real (infinity)
import Data.List (partition)
import Control.Comonad.Store (ComonadStore(pos))

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


rateGame :: GameState -> Int
rateGame gs@(_,_,bxs) = case checkWinner gs of
                            Nothing -> let (p1Boxes, p2Boxes) = partition (\(Box point player) -> player == PlayerOne) bxs
                                           p1Score = length p1Boxes
                                           p2Score = length p2Boxes
                                        in p1Score - p2Score
                            Just (Winner PlayerOne) -> length bxs * 10
                            Just (Winner PlayerTwo) -> length bxs * (-10)

-- when rating_mvs is calculated the correct working banks on the fact that whenever a move from the list of legalmoves is made it wouldn't return a Nothing. If it returns nothing zipping the rating to moves wouldn't work because the length of ratings might be less then length of moves.
whoMightWin :: GameState -> Int -> (Int, Maybe Move)
whoMightWin gs@(trn,_,_) depth = 
    let recur_depth:: Int -> GameState -> Int
        recur_depth 0 xgs = rateGame xgs
        recur_depth depth xgs@(xtrn,_,_)= 
          let possibleGS =  mapMaybe (makeMove xgs) (findLegalMoves xgs)
              ratings = map (recur_depth (depth-1)) possibleGS
          in if trn == PlayerOne then maximum ratings else minimum ratings
    in case checkWinner gs of
      Nothing -> 
        let possibleMvs = findLegalMoves gs
            rating_mvs :: [(Int,Maybe Move)]
            rating_mvs = zip (map (recur_depth depth) (mapMaybe (makeMove gs) possibleMvs)) (map Just possibleMvs)
        in if trn == PlayerOne then maximum rating_mvs else minimum rating_mvs 
      Just (Winner PlayerOne) -> (rateGame gs, Nothing)
      Just (Winner PlayerTwo) -> (rateGame gs, Nothing)
