module Game
  ( game
  ) where

import Prelude

import Effect (Effect)
import Game.Config (config)
import Game.Scenes (scenes)
import Phina (enableStats, newGame, runGame)

game ∷ Effect Unit
game = newGame config scenes >>= enableStats >>= runGame
