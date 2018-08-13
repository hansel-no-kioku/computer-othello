module Game.Scenes
  ( scenes
  ) where

import Prelude

import Effect.Class (liftEffect)
import Game.Computer.Anticipator.Worker (Anticipator)
import Game.Scenes.Title (titleScene)
import Phina (GameScenes(..), StartScene(..), foreverAsync, launchAsync', popup, splashScene)
import SimpleWorker (Worker, newWorker)

scenes ∷ GameScenes
scenes = SceneListDefault Main $ \_ _ → launchAsync' do
  (anticipator ∷ Worker Anticipator) ← liftEffect $ newWorker "js/worker.js"
  _ ← popup splashScene {}
  foreverAsync $ titleScene anticipator
