module Game.Scenes.Title
  ( titleScene
  ) where

import Prelude

import Data.FoldableWithIndex (forWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.FSM (Machine, machine, receive, send)
import Game.Computer.Anticipator.Worker (Anticipator)
import Game.Othello.Board (Side(..))
import Game.Player.Types (PlayerType(..), Wiseness(..))
import Game.Scenes.Main (mainScene)
import Game.Scenes.Title.BgPieces (addBgPieces, splashBgPieces)
import Game.Style (layout, style)
import Phina (Async, Button, DisplayScene, addChildToB, build, getSize, getSpan, getUnit, liftBuilder, make, newButton, newDisplayElement, newGrid, newLabel, number, onPushB, popup, setInteractive, setPositionB, setPositionB', setProps, setPropsB, toSceneHandle, update)
import Phina.Accessor (setBackgroundColor, setText)
import SimpleWorker (Worker)


titleScene ∷ Worker Anticipator → Async DisplayScene Unit
titleScene anticipator =
  void $ flip popup {} $ toSceneHandle $ \_ exit scene → do
    _ ← setBackgroundColor style.titleScene.backgroundColor scene

    bgPieces ← addBgPieces scene

    pos ← layout.titleScene.title scene
    _ ← make (newLabel style.title) do
      setPositionB pos
      addChildToB scene

    state ← titleState

    buttons ← addSettingPanel scene state

    pos' ← layout.titleScene.startButton scene
    startButton ← make (newButton style.startButton) do
      setPositionB pos'
      onPushB \_ → send Start state
      addChildToB scene

    receive state case _ of
      Setting players → updateSettingButtons players buttons

      Exit players → launchAff_ do
        liftEffect do
          lockSettingButtons buttons
          _ ← setInteractive false startButton
          void $ setProps style.startButtonPushed startButton
        splashBgPieces scene bgPieces
        void $ scene # build do
          _ ← mainScene {anticipator, players}
          liftBuilder $ update $ exit {}

      _ → pure unit

    pure scene

--

type SettingButtons = Map Side (Map PlayerType Button)

getButton ∷ Side → PlayerType → SettingButtons → Maybe Button
getButton side typ buttons = lookup side buttons >>= lookup typ

addSettingPanel ∷ DisplayScene → Machine Input Output → Effect SettingButtons
addSettingPanel scene state = do
  panel ←  make (newDisplayElement style.settingPanel) do
    pos ← liftEffect $ layout.titleScene.settingPanel scene
    setPositionB pos
    addChildToB scene

  size ← getSize panel

  let
    calcOffset w n = - w / 2.0 + w / 2.0 / number n
    newGrid' w n = newGrid w n false $ calcOffset w n
    gridX = newGrid' size.width 2
    gridY = newGrid' size.height 4
    buttonSize = {width: getUnit gridX, height: getUnit gridY}
    playerTypes = [Human, Com Lv1, Com Lv2, Com Lv3]

  map fromFoldable $ sequence $
    [Dark, Light] # mapWithIndex \sx side → do
      let x = getSpan sx gridX

      row ← map fromFoldable $ sequence $
        playerTypes # mapWithIndex \sy typ → do
          let y = getSpan sy gridY
          button ← make (newButton $ style.selectButton buttonSize) do
            setPositionB' x y
            update $ setText $ show typ
            setPropsB style.selectButtonOff
            onPushB \_ → send (Select side typ) state
            addChildToB panel
          pure $ Tuple typ button

      pure $ Tuple side row


updateSettingButtons ∷ Map Side PlayerType → SettingButtons → Effect Unit
updateSettingButtons players buttons =
  forWithIndex_ buttons \side row →
    forWithIndex_ row \typ button →
      let selected = maybe false (_ == typ) $ lookup side players
          props = if selected then style.selectButtonOn side
                              else style.selectButtonOff
      in setProps props button

lockSettingButtons ∷ SettingButtons → Effect Unit
lockSettingButtons = traverse_ (traverse_ $ setInteractive false)


--

data Input  = NoInput
            | Select Side PlayerType
            | Start

data Output = NoOutput
            | Setting (Map Side PlayerType)
            | Exit (Map Side PlayerType)

titleState ∷ Effect (Machine Input Output)
titleState = machine step initialState NoInput NoOutput
  where
    initialState = fromFoldable [Tuple Dark Human, Tuple Light (Com Lv2)]

    step (Select side typ) state = stepSelect side typ state
    step Start state = pure $ Tuple (Just $ Exit state) state
    step _ state = pure $ Tuple (Just $ Setting state) state

    stepSelect side typ state = ado
      let nextState = insert side typ state
      in  Tuple (Just $ Setting nextState) nextState
