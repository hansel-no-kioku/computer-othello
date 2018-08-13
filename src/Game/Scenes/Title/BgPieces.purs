module Game.Scenes.Title.BgPieces
  ( addBgPieces
  , splashBgPieces
  ) where


import Prelude

import Control.Parallel (parSequence_)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Random (randomRange)
import Effect.Ref (Ref, new, read, write)
import Game.Othello.Board (Side(..))
import Game.Style (style)
import Math (sqrt)
import Phina (class Readable, CircleShape, DisplayScene, Duration, Position, Size, Vector2, addChildToB, animateB, getCenterPos, getPosition, getProps, getSize, make, newCircleShape, newRectangleShape, randomVector2, remove, setPosition, setPositionB, setSizeB, setUpdater, toSec, up, (*~))
import Phina.Accessor (getDeltaTime)


type BgPiece = {shape ∷ CircleShape, velocity ∷ Ref Vector2}


addBgPieces ∷ DisplayScene → Effect (Array BgPiece)
addBgPieces scene = do
  size ← getSize scene
  pos ← getCenterPos scene
  stage ← make (newRectangleShape style.bgPiecesStage) do
    setSizeB size
    setPositionB pos
    addChildToB scene

  for (1..style.bgPiece.num) \_ → newBgPiece stage

  where
    newBgPiece stage = do
      fluctuation ← randomRange 0.9 1.1
      shape ← make (newCircleShape $ style.piece style.bgPiece.width Dark) do
        animateB $ style.bgPieceAnimation fluctuation
        addChildToB stage

      velocity ← new up
      let piece = {shape, velocity}

      areaSize ← getAreaSize stage
      resetBgPiece areaSize piece

      _ ← shape # setUpdater \app _ → do
        delta ← getDeltaTime app
        vel ← read velocity
        pos ← getPosition shape
        _ ← setPosition (pos + calcMove delta vel) shape

        hit ← hitTestBgPiece areaSize shape
        unless hit $ resetBgPiece areaSize piece

      pure piece


    resetBgPiece areaSize piece = do
      pos ← getInitialPosition areaSize
      _ ← setPosition pos piece.shape
      let speed = style.bgPiece.speed
      velocity ← randomVector2 speed speed
      write velocity piece.velocity


    getInitialPosition areaSize = do
      let range = (areaSize.width + areaSize.height) * 2.0
      r ← randomRange 0.0 range
      let pos = if r < areaSize.width
                  then {x: r, y: 0.0}
                  else
                    let r2 = r - areaSize.width
                    in
                      if r2 < areaSize.height
                        then {x: areaSize.width, y: r2}
                        else
                          let r3 = r2 - areaSize.height
                          in
                            if r3 < areaSize.width
                              then {x: r3, y: areaSize.height}
                              else
                                let r4 = r3 - areaSize.width
                                in  {x: 0.0, y: r4}
      pure $ pos - {x: areaSize.width / 2.0, y: areaSize.height / 2.0}


splashBgPieces ∷ DisplayScene → Array BgPiece → Aff Unit
splashBgPieces scene pieces = do
  areaSize ← liftEffect $ getAreaSize scene
  parSequence_ $ splashPiece areaSize <$> pieces

  where
    splashPiece areaSize piece = makeAff \f → do
      vel ← getSplashVelocity piece
      _ ← piece.shape # setUpdater \app _ → do
        delta ← getDeltaTime app
        pos ← getPosition piece.shape
        _ ← setPosition (pos + calcMove delta vel) piece.shape

        hit ← hitTestBgPiece areaSize piece.shape
        unless hit do
          _ ← remove piece.shape
          f $ Right unit

      pure nonCanceler


    getSplashVelocity piece = do
      pos ← (\p → if zero == p then up else p) <$> getPosition piece.shape
      let length = sqrt $ pos.x * pos.x + pos.y * pos.y
          rate = style.bgPiece.splashSpeed / length
      pure {x: pos.x * rate, y: pos.y * rate}


getAreaSize ∷ ∀ a. Readable a Size ⇒ a → Effect Size
getAreaSize stage = ado
  size ← getSize stage
  let margin = style.bgPiece.width * 2.0
  in  size + {width: margin, height: margin}


calcMove ∷ Duration → Vector2 → Position
calcMove duration velocity = (toSec duration) *~ velocity


type Area = {left ∷ Number, right ∷ Number, top ∷ Number, bottom ∷ Number}

hitTestBgPiece ∷ Size → CircleShape → Effect Boolean
hitTestBgPiece size shape = ado
  let width = size.width / 2.0
      height = size.height / 2.0
  {left, right, top, bottom} ← getProps shape ∷ Effect Area
  in   left <= width
    && right >= -width
    && top >= -height
    && bottom <= height
