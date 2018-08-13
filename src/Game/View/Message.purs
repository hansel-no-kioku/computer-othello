module Game.View.Message
  ( Event(..)
  , Msg(..)
  ) where

import Data.Map (Map)
import Game.Othello.Board (Board, Side)
import Game.Player.Types (PlayerType)


data Event = Move | Pass

data Msg  = Init
          | Ready (Map Side PlayerType)
          | Turn Event Board Side
          | Finish Board
