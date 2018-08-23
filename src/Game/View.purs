module Game.View
  ( newView
  ) where

import Prelude

import Control.Parallel (parSequence_)
import Data.Array (head)
import Data.Either (Either(..))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (pow, toNumber)
import Data.Map (Map, empty, fromFoldable)
import Data.Matrix (Matrix, catMaybes, replicate, zipWith, zipWithA)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.FSM (Machine, machineAff)
import Game.Othello.Board (Board, Piece(..), Side(..), getScore)
import Game.Player.Message as P
import Game.Player.Types (PlayerType)
import Game.Scenes.Result (resultScene)
import Game.Style (layout, style)
import Game.View.Message as V
import Game.View.Score (ScoreDisplay, addScoreDisplay, updateScoreDisplay)
import Game.View.TurnAnimation (TurnAnimation, newTurnAnimation, setActive)
import Math (sqrt)
import Phina (CircleShape, DisplayScene, RectangleShape, addChild, addChildToB, animate, animateB, call, color, easeDefault, getCenterPos, getSize, getSpan, getUnit, launchAsync', make, newCircleShape, newGrid, newLabel, newRectangleShape, sec, setPosition, setPositionB, setPositionB', setSizeB, to, toDisplayElement, toParams)
import Phina.Accessor (getWidth, setBackgroundColor)


type Cell = Tuple RectangleShape (Maybe (Tuple Side CircleShape))

type ViewState =
  { scene ∷ DisplayScene
  , stage ∷ RectangleShape
  , cells ∷ Matrix Cell
  , turnAnimation ∷ Map Side TurnAnimation
  , scoreDisplay ∷ ScoreDisplay
  , players ∷ Map Side PlayerType
  }


newView ∷ DisplayScene → (Effect Unit) → Effect (Machine V.Msg P.Msg)
newView scene exit = do
  state ← initialState scene
  machineAff step state V.Init P.Init
  where
    step (V.Ready players) state = stepReady players state
    step (V.Turn event board side) state = stepTurn event board side state
    step (V.Finish board) state = stepFinish exit board state
    step _ state = pure $ Tuple Nothing state


initialState ∷ DisplayScene → Effect ViewState
initialState scene = do
  _ ← setBackgroundColor (color "transparent") scene

  size ← getSize scene

  stage ← make (newRectangleShape style.stage) do
    setSizeB size
    addChildToB scene

  pos ← layout.mainScene.board scene
  (Tuple boardShape cells) ← newBoard size.width
  _ ← setPosition pos boardShape
  _ ← addChild boardShape stage

  scoreDisplay ← addScoreDisplay scene

  let turnAnimation = empty
      players = empty

  pure {scene, stage, cells, turnAnimation, scoreDisplay, players}


stepReady
   ∷ Map Side PlayerType
  → ViewState
  → Aff (Tuple (Maybe P.Msg) ViewState)
stepReady players state = do
  turnAnimation ← liftEffect $ sequence $
                      mapWithIndex addNameWithTurnAnimation $ show <$> players

  makeAff \f → do
    _ ← state.stage # animate do
      to (toParams {alpha: 1.0}) (sec style.sceneTransitionTime) easeDefault
      call \s → f (Right unit) $> s
    pure nonCanceler

  let msg = P.Ready $ (toDisplayElement <<< fst) <$> state.cells
      nextState = state{turnAnimation = turnAnimation, players = players}

  pure $ Tuple (Just msg) nextState

  where
    addNameWithTurnAnimation side name = do
      let newLabel' = newLabel $ style.nameLabel side name
      label ← make newLabel' do
        pos ← liftEffect $ layout.mainScene.nameLabel side state.scene
        setPositionB pos
        addChildToB state.scene
      newTurnAnimation label style.turnAnimation


stepTurn
   ∷ V.Event
  → Board
  → Side
  → ViewState
  → Aff (Tuple (Maybe P.Msg) ViewState)
stepTurn event board side state = do
  liftEffect $ traverse_ (setActive false) state.turnAnimation
  state' ← updateView event board state
  liftEffect $ traverseWithIndex_ doTurnAnimation state.turnAnimation
  pure $ Tuple (Just $ P.Turn board side) state'

  where
    updateView V.Move b s = updateBoard b s
    updateView V.Pass _ s = displayPass state $> s

    doTurnAnimation side' turnAnimation =
      when (side' == side) $ setActive true turnAnimation


updateBoard ∷ Board → ViewState → Aff ViewState
updateBoard board state = do
  let news = getAddedPieces board state.cells
      flips = getFlippedPieces board state.cells

  cells ← liftEffect $ addPieces news state.cells
  cells' ← flipPieces news flips cells

  let scores = fromFoldable $
                (\side → Tuple side $ getScore board side) <$> [Dark, Light]
  scoreDisplay ← liftEffect $ updateScoreDisplay scores state.scoreDisplay

  pure $ state{cells = cells', scoreDisplay = scoreDisplay}

  where
    getAddedPieces = zipWith \p (Tuple _ p') →
      case p, p' of
        Piece side, Nothing → Just side
        _, _ → Nothing

    getFlippedPieces = zipWith \p (Tuple _ p') →
      case p, p' of
        Piece side, Just (Tuple side' _) | side /= side' → Just side
        _, _ → Nothing

    addPieces = zipWithA \s (Tuple cell p) →
      Tuple cell <$> case s, p of
                      Just side, _ → do
                        p' ← newPiece cell side
                        _ ← addChild p' cell
                        pure $ Just $ Tuple side p'
                      _, p' → pure p'


    flipPieces news flips cells = do
      let
        flips' = flipShapes flips cells
        origin = head $ catMaybes $ mapWithIndex (\pos s → s $> pos) news
      parSequence_ $ catMaybes $ mapAnimation origin flips'
      pure $ zipFlipPieces flips cells

    flipShapes = zipWith \s (Tuple _ p) →
      case s, p of
        Just side, Just (Tuple _ p') → Just $ Tuple side p'
        _, _ → Nothing

    mapAnimation origin = mapWithIndex \pos s → s <#>
      \(Tuple side p) →
        let dist = maybe 0.0 (\o → calcDist pos o) origin
        in makeAff \f →
          let tween = style.flipAnimation (dist - 1.0) side $ f $ Right unit
          in  animate tween p $> nonCanceler

    calcDist a b = sqrt $ toNumber $ pow (a.x - b.x) 2 + pow (a.y - b.y) 2

    zipFlipPieces = zipWith \s c@(Tuple cell p) →
      case s, p of
        Just side, Just (Tuple _ p') → Tuple cell $ Just $ Tuple side p'
        _, _ → c


displayPass ∷ ViewState → Aff Unit
displayPass state = makeAff \f → do
  center ← getCenterPos state.scene
  width ← getWidth state.scene
  _ ← make (newLabel style.passLabel) do
    setPositionB center
    animateB $ style.passAnimation width $ f $ Right unit
    addChildToB state.stage
  pure nonCanceler


stepFinish
   ∷ Effect Unit
  → Board
  → ViewState
  → Aff (Tuple (Maybe P.Msg) ViewState)
stepFinish exit board state = makeAff \f → do
  traverse_ (setActive false) state.turnAnimation

  center ← getCenterPos state.scene

  let addScore side playerType = Tuple playerType $ getScore board side
      result = mapWithIndex addScore state.players
      f' = do
        _ ←  state.scene # launchAsync' do
          _ ← resultScene result
          liftEffect exit
        f $ Left $ error "finish"

  label ← make (newLabel style.finishLabel) do
    setPositionB center
    animateB $ style.finishAnimation f'
    addChildToB state.stage

  pure nonCanceler

--

newBoard ∷ Number → Effect (Tuple RectangleShape (Matrix Cell))
newBoard width = do
  board ← newRectangleShape $ style.board width
  boardWidth ← getWidth board
  let
    offset = boardWidth / 8.0 / 2.0 - boardWidth / 2.0
    grid = newGrid boardWidth 8 false offset
    cellWidth = getUnit grid
    mapCell = mapWithIndex \{x, y} _ → do
      cell ← make (newRectangleShape $ style.cell cellWidth) do
        setPositionB' (getSpan x grid) (getSpan y grid)
        addChildToB board
      pure $ Tuple cell Nothing
  cells ← sequence $ mapCell $ replicate 8 8 cellWidth

  pure $ Tuple board cells


newPiece ∷ RectangleShape → Side → Effect CircleShape
newPiece cell side = do
  width ← getWidth cell
  newCircleShape $ style.piece width side

