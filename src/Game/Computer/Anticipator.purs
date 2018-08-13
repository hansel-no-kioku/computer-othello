module Game.Computer.Anticipator
  ( calcScore
  ) where

import Prelude

import Data.Array (length, null)
import Data.Foldable (maximum, sum)
import Data.Matrix (filter, zipWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Game.Othello.Board (Board, CellType(..), Piece(..), Side(..), cellTypes, enemy, getMovablePos, getSafePlaces, move)


calcScore ∷ Int → Board → Side → Int → Int
calcScore cost board side passCount | cost > 0 =
  let nextSide = enemy side
      movable = getMovablePos board nextSide
  in  if null movable
        then
          if passCount > 0
            then calcScore 0 board side 2
            else negate $ calcScore (cost - 1) board nextSide 1
        else
          let cost' = cost / length movable
          in  negate $ selectMax (calcScoreEach cost' nextSide <$> movable)

  where
    calcScoreEach cost' side' pos =
      case move board side' pos of
        Just board' → calcScore (cost' - 1) board' side' 0
        Nothing → 0

    selectMax = fromMaybe 0 <<< maximum

calcScore _ board side passCount = do
  if passCount >= 2
    then calcLastScore
    else if isFinished board
      then calcLastScore
      else calcNowScore

  where
    isFinished board' =
         (null $ getMovablePos board' Dark)
      && (null $ getMovablePos board' Light)

    calcLastScore =
      let
        countPiece side' = length <<< filter (_ == Piece side')
        friendPieceScore = countPiece side board
        enemyPieceScore = countPiece (enemy side) board
        score = friendPieceScore - enemyPieceScore
      in
             if score > 0 then score * winPieceScore + winningScore
        else if score < 0 then score * winPieceScore - winningScore
        else                   score

    calcNowScore =
      let
        friendPieceScore = calcSideScore side board
        enemyPieceScore = calcSideScore (enemy side) board
      in
        friendPieceScore - enemyPieceScore

    calcSideScore side' board' =
      let
        movableNum = length $ getMovablePos board' side'
        safePlaces = getSafePlaces board' side'
        cellTypes' = zipWith (\s t → if s then Corner else t) safePlaces cellTypes
        typedBoard = zipWith Tuple board' cellTypes'
        pieceScore = sum $ cellTypeScore
                        <$> snd
                        <$> filter (\(Tuple p _) → p == Piece side') typedBoard
      in
        pieceScore + movableNum * movableScore

    cellTypeScore = case _ of
      Corner  → 200
      NCorner → -100
      Corner2 → 20
      Edge    → 10
      Edge2   → 2
      Inside  → 1

    movableScore = 5
    winningScore = 20000
    winPieceScore = 100
