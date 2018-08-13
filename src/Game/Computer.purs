module Game.Computer
  ( newComputer
  ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Primitivizable (evolve, primitivize)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.FSM (Machine, machineAff)
import Game.Computer.Anticipator.Worker (Anticipator, Result)
import Game.Othello.Board (Board, Pos, Side)
import Game.Othello.Message as O
import Game.Player.Message as P
import Game.Player.Types (Wiseness(..))
import SimpleWorker (Worker, request)


type Computer =
  { wiseness ∷ Wiseness
  , side ∷ Side
  , anticipator ∷ Worker Anticipator
  }


newComputer
   ∷ Worker Anticipator
  → Wiseness
  → Side
  → Effect (Machine P.Msg O.Msg)
newComputer anticipator wiseness side =
  machineAff step {wiseness, side, anticipator} P.Init O.Init
  where
    step (P.Ready _) state = pure $ Tuple (Just O.OK) state
    step (P.Turn board side') state = stepTurn board side' state
    step _ state = pure $ Tuple Nothing state


stepTurn ∷ Board → Side → Computer → Aff (Tuple (Maybe O.Msg) Computer)
stepTurn board side state = do
  if side == state.side then think board state
                        else pure $ Tuple Nothing state


think ∷ Board → Computer → Aff (Tuple (Maybe O.Msg) Computer)
think board state = thinkMove board state >>= maybe pass move
  where
    pass = pure $ Tuple (Just $ O.Pass state.side) state
    move pos = pure $ Tuple (Just $ O.Move state.side pos) state


thinkMove ∷ Board → Computer → Aff (Maybe Pos)
thinkMove board state = do
  let args = {board, side: state.side, depth: getCalcType state.wiseness}
  result ← request (primitivize args) state.anticipator
  pure $ (unwrap ∷ Result → Maybe Pos) $ evolve result

  where
    getCalcType Lv1 = 0
    getCalcType Lv2 = 50
    getCalcType Lv3 = 250
