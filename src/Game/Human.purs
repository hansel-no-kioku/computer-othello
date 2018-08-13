module Game.Human
  ( newHumanPlayer
  ) where

import Prelude

import Control.Parallel (parOneOf)
import Data.Array (catMaybes, null, unzip)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Matrix (Matrix, empty, index)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.FSM (Machine, machineAff)
import Game.Othello.Board (Board, Side, enemy, getFlippablePieces, getMovablePos)
import Game.Othello.Message as O
import Game.Player.Message as P
import Game.Style (style)
import Phina (DisplayElement, addChildB, addChildToB, make, newCircleShape, onPointEnd, onPointOut, onPointOver, onRemoved, remove, setInteractiveB, update)
import Phina.Accessor (getWidth, setAlpha, setVisible)

type Human =
  { side ∷ Side
  , cells ∷ Matrix DisplayElement
  }


newHumanPlayer ∷ Side → Effect (Machine P.Msg O.Msg)
newHumanPlayer side = machineAff step initialState P.Init O.Init
  where
    initialState = {side, cells: empty}
    step (P.Ready cells) state = pure $ Tuple (Just O.OK) state{cells = cells}
    step (P.Turn board side') state = stepTurn board side' state
    step _ state = pure $ Tuple Nothing state


stepTurn ∷ Board → Side → Human → Aff (Tuple (Maybe O.Msg) Human)
stepTurn board side state = do
  if side == state.side then waitMove board state
                        else pure $ Tuple Nothing state


waitMove ∷ Board → Human → Aff (Tuple (Maybe O.Msg) Human)
waitMove board state = do
  let
    suggestions = catMaybes $ makeSuggestion <$> addFlippable
                                             <$> getMovablePos board state.side
  if null suggestions
    then
      pure $ Tuple (Just $ O.Pass state.side) state
    else do
      icons ← liftEffect $ sequence (addIcons state.side <$> suggestions)
      msg ← parOneOf $ (\{pos, icon} → wait state.side pos icon) <$> icons
      liftEffect $ traverse_ (_.icon >>> remove) icons
      pure $ Tuple (Just msg) state

  where
    addFlippable pos = Tuple pos $ getFlippablePieces board state.side pos

    makeSuggestion (Tuple pos flippable) = ado
      cell ← index pos.x pos.y state.cells
      flippableCells ← sequence $ (\fpos → index fpos.x fpos.y state.cells)
                                <$> flippable
      in {pos, cell, flippableCells}

    addIcons side {pos, cell, flippableCells} = do
      width ← getWidth cell
      icon ← make (newCircleShape $ style.piece width side) do
        update $ setAlpha style.movableIconAlpha
        setInteractiveB true
        addChildToB cell
      addFlippableIcons side icon flippableCells
      pure $ {pos, icon}

    addFlippableIcons side icon flippableCells = do
      Tuple ficons tweeners ← unzip <$> liftEffect do
        for flippableCells \fcell → do
          width ← getWidth fcell

          ficon ← newCircleShape $ style.piece width $ enemy side
          tweener ← style.suggestAnimation side ficon

          mask ← make (newCircleShape $ style.suggestMask width) do
            update $ setVisible false
            addChildB ficon
            addChildToB fcell

          let play = setVisible true mask *> tweener.play
              stop = setVisible false mask *> tweener.stop

          pure $ Tuple mask {play, stop}

      _ ← onPointOver (\_ _ → traverse_ _.play tweeners) icon
      _ ← onPointOut (\_ _ → traverse_ _.stop tweeners) icon
      _ ← onRemoved (\_ → traverse_ remove ficons) icon

      pure unit

    wait side pos icon = makeAff \f →
      onPointEnd (\e _ → when e.over $ f $ Right $ O.Move state.side pos) icon
      $> nonCanceler
