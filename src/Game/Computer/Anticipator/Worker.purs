module Game.Computer.Anticipator.Worker
  ( Anticipator
  , Result(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Primitivizable (class Primitivizable)
import Game.Othello.Board (Pos)
import SimpleWorker (class SimpleWorker)


foreign import data Anticipator ∷ Type

newtype Result = Result (Maybe Pos)
derive instance newtypeResult ∷ Newtype Result _

instance primitivizableResult
   ∷ Primitivizable Result {isValid ∷ Boolean, pos ∷ {x ∷ Int, y ∷ Int}}
   where
    primitivize (Result (Just pos)) = {isValid: true, pos}
    primitivize (Result Nothing) = {isValid: false, pos: zero}
    evolve {isValid: true, pos} = Result $ Just pos
    evolve {isValid: false} = Result Nothing


instance simpleWorkerAnticipator ∷ SimpleWorker Anticipator
  { board ∷ {width ∷ Int, values ∷ Array String}
  , side ∷ String
  , depth ∷ Int
  }
  { isValid ∷ Boolean
  , pos ∷ {x ∷ Int, y ∷ Int}
  }
