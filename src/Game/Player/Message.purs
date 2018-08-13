module Game.Player.Message
  ( Msg(..)
  ) where

import Data.Matrix (Matrix)
import Game.Othello.Board (Board, Side)
import Phina (DisplayElement)

data Msg  = Init
          | Ready (Matrix DisplayElement)
          | Turn Board Side
