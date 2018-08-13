module Game.Scenes.Main
  ( mainScene
  ) where

import Prelude

import Data.Map (Map, lookup)
import Data.Maybe (fromMaybe)
import Effect.FSM (connect, send)
import Game.Computer (newComputer)
import Game.Computer.Anticipator.Worker (Anticipator)
import Game.Human (newHumanPlayer)
import Game.Othello (newOthello)
import Game.Othello.Board (Side(..))
import Game.Othello.Message (Msg(..))
import Game.Player.Types (PlayerType(..))
import Game.View (newView)
import Phina (Async, DisplayScene, popup, toSceneHandle)
import SimpleWorker (Worker)


mainScene
   ∷ {anticipator ∷ Worker Anticipator, players ∷ Map Side PlayerType}
  → Async DisplayScene {}
mainScene = popup $ toSceneHandle setupScene
  where
    setupScene {anticipator, players} exit scene = do
      o ← newOthello
      v ← newView scene $ void $ exit {} scene

      p1 ← getPlayer (fromMaybe Human $ lookup Dark players) Dark
      p2 ← getPlayer (fromMaybe Human $ lookup Light players) Light

      _ ← connect o v
      _ ← connect v p1
      _ ← connect v p2
      _ ← connect p1 o
      _ ← connect p2 o

      scene <$ send (Start players) o

      where
        getPlayer Human = newHumanPlayer
        getPlayer (Com wiseness) = newComputer anticipator wiseness
