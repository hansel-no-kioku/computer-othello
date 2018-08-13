module Game.View.Score
  ( ScoreDisplay
  , addScoreDisplay
  , updateScoreDisplay
  ) where


import Prelude

import Data.FoldableWithIndex (traverseWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (pow)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Game.Othello.Board (Side(..))
import Game.Style (layout, style)
import Phina (DisplayScene, Label, addChildToB, animate, make, newLabel, setPositionB, setTextB)


type ScoreDisplay = Map Side (Tuple Int (Array Label))


addScoreDisplay ∷ DisplayScene → Effect ScoreDisplay
addScoreDisplay scene =
            fromFoldable <$> (sequence $ newScoreLabels <$> [Dark, Light])

  where
    newScoreLabels side = ado
      labels ← sequence $ addDigitLabel side <$> [0, 1]
      in Tuple side (Tuple 2 labels)

    addDigitLabel side digit =
      make (newLabel $ style.digit side digit) do
        pos ← liftEffect $ layout.mainScene.scoreDigit side digit scene
        setTextB if digit > 0 then " " else "2"
        setPositionB pos
        addChildToB scene


updateScoreDisplay ∷ Map Side Int → ScoreDisplay → Effect ScoreDisplay
updateScoreDisplay scores scoreDisplay = do
  sequence $ mapWithIndex updateScore scoreDisplay

    where
      updateScore side state@(Tuple nowScore labels) =
        case lookup side scores of
          Just nextScore → ado
            traverseWithIndex_ (updateDigit nowScore nextScore) labels
            in Tuple nextScore labels
          Nothing → pure $ state

      updateDigit nowScore nextScore i label = do
        let nowDigit = (nowScore / pow 10 i) `mod` 10
            nextDigit = (nextScore / pow 10 i) `mod` 10
            str = if i > 0 && nextDigit == 0 then " " else show nextDigit
        when (nowDigit /= nextDigit) do
          void $ animate (style.digitAnimation str) label
