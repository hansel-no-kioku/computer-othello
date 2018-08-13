module Game.Player.Types
  ( Wiseness(..)
  , PlayerType(..)
  , getShortName
  ) where

import Prelude


data Wiseness = Lv1
              | Lv2
              | Lv3

derive instance eqWiseness ∷ Eq Wiseness
derive instance ordWiseness ∷ Ord Wiseness

instance showWiseness ∷ Show Wiseness where
  show Lv1 = "Lv1"
  show Lv2 = "Lv2"
  show Lv3 = "Lv3"


data PlayerType = Human
                | Com Wiseness

derive instance eqPlayerType ∷ Eq PlayerType
derive instance ordPlayerType ∷ Ord PlayerType

instance showPlayerType ∷ Show PlayerType where
  show Human = "Player"
  show (Com wiseness) = "Computer " <> show wiseness

getShortName ∷ PlayerType → String
getShortName Human = "Player"
getShortName (Com wiseness) = "Com. " <> show wiseness
