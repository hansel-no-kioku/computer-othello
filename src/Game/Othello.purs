module Game.Othello
  ( newOthello
  ) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.FSM (Machine, machine)
import Game.Othello.Board (Board, Pos, Side(..), enemy, getMovablePos, initialBoard, move)
import Game.Othello.Message as O
import Game.View.Message as V


type Othello =
  { board ∷ Board
  , side ∷ Side
  , okCount ∷ Int
  }

newOthello ∷ Effect (Machine O.Msg V.Msg)
newOthello = machine step initialState O.Init V.Init
  where
    initialState = {board: initialBoard, side: Dark, okCount: 0}

    step (O.Start players) state = pure $ Tuple (Just $ V.Ready players) state
    step O.OK state = stepOK state
    step (O.Move side pos) state = stepMove side pos state
    step (O.Pass side) state = stepPass side state
    step _ state = pure $ Tuple Nothing state


stepOK ∷ Othello → Effect (Tuple (Maybe V.Msg) Othello)
stepOK state = ado
  let nextState = state{okCount = state.okCount + 1}
  in  if nextState.okCount >= 2
        then Tuple (Just $ V.Turn V.Move state.board state.side) nextState
        else Tuple Nothing nextState


stepMove ∷ Side → Pos → Othello → Effect (Tuple (Maybe V.Msg) Othello)
stepMove side pos state =
  if side == state.side
    then
      case move state.board side pos of
        Just board' → ado
          let side' = enemy side
              state' = state{board = board', side = side'}
          in  Tuple (Just $ V.Turn V.Move board' side') state'
        Nothing → pure $ Tuple Nothing state
    else
      pure $ Tuple Nothing state


stepPass ∷ Side → Othello → Effect (Tuple (Maybe V.Msg) Othello)
stepPass side state = ado
  let side' = enemy side
      msg = if null $ getMovablePos state.board $ enemy side
                then V.Finish state.board
                else V.Turn V.Pass state.board side'
      state' = state{side = side'}
  in Tuple (Just msg) state'
