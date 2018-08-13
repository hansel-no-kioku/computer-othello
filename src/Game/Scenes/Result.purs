module Game.Scenes.Result
  ( resultScene
  ) where

import Prelude

import Data.Foldable (intercalate)
import Data.Map (Map, lookup)
import Data.Maybe (maybe)
import Data.Tuple (Tuple, fst, snd)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write)
import Game.Config (config)
import Game.Othello.Board (Side(..))
import Game.Player.Types (PlayerType, getShortName)
import Game.Style (style)
import Phina (Async, Builder, DisplayScene, ResultScene, build, color, popup, setPropsB, setUpdaterB, toSceneHandle')
import Phina.Accessor (getFps, setBackgroundColor)


type Result = Map Side (Tuple PlayerType Int)


resultScene ∷ Result → Async DisplayScene {}
resultScene result = do
  let score = getScoreStr
      message = getMessageStr
  popup (toSceneHandle' setupScene) {score, message, hashtags}

  where
    getScoreStr =
      let getScore side = maybe 0 snd $ lookup side result
      in  intercalate " - " $ show <$> getScore <$> [Dark, Light]

    getMessageStr =
      let getPlayerName side =
                    maybe "Unknown" (fst >>> getShortName) $ lookup side result
          playerNames = intercalate " vs " $ getPlayerName <$> [Dark, Light]
      in  playerNames <> " in " <> config.title

    hashtags = "phina_js,phinajs,game,purescript,javascript"

    bgcolor alpha = color $ "rgba(0, 0, 0, " <> show alpha <> ")"

    setupScene _ _ = build do
      setPropsB {backgroundColor: bgcolor 0.0} ∷ Builder ResultScene Unit

      alphaRef ← liftEffect $ new 0.0

      setUpdaterB \app scene → do
        alpha ← read alphaRef
        when (alpha < style.resultSceneAlpha) do
          fps ← getFps app
          let delta = style.resultSceneAlpha / style.sceneTransitionTime / fps
              alpha' = min style.resultSceneAlpha $ alpha + delta
          _ ← setBackgroundColor (bgcolor alpha') scene
          write alpha' alphaRef
