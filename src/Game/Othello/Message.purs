module Game.Othello.Message
  ( Msg(..)
  ) where

import Data.Map (Map)
import Game.Othello.Board (Pos, Side)
import Game.Player.Types (PlayerType)


data Msg  = Init
          | Start (Map Side PlayerType)
          | OK
          | Move Side Pos
          | Pass Side
