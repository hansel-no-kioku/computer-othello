module Game.Computer.Anticipator.Server
  ( main
  ) where


import Prelude

import Data.Array (catMaybes, length, null)
import Data.Foldable (intercalate, maximumBy)
import Data.Function (on)
import Data.Matrix (filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Primitivizable (evolve, primitivize)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomInt)
import Game.Computer.Anticipator (calcScore)
import Game.Computer.Anticipator.Worker (Anticipator, Result(..))
import Game.Othello.Board (Board, Pos, Side(..), getMovablePos, getSafePlaces, move)
import SimpleWorker (response)
import Type.Prelude (Proxy(..))


main ∷ Effect Unit
main = response (Proxy ∷ Proxy Anticipator)
  \args → (primitivize <<< Result) <$> think (evolve args)

think ∷ {board ∷ Board, side ∷ Side, depth ∷ Int} → Effect (Maybe Pos)
think {board, side, depth} =
  let movable = getMovablePos board side
  in  if null movable then pure Nothing
                      else select movable

  where
    select ∷ Array Pos → Effect (Maybe Pos)
    -- select movable = selectBest <$> (calcScore' $ catMaybes $ move' <$> movable)
    select movable =
      tapLogSafe =<<
        selectBest <$> (tapLog =<< (calcScore' $ catMaybes $ move' <$> movable))

    move' pos = Tuple pos <$> move board side pos

    calcScore' =
      let addRandom score = (score + _) <$> randomInt 0 5
          calc board' = addRandom $ calcScore depth board' side 0
      in  sequence <<< map (sequence <<< map calc)

    selectBest = map fst <<< maximumBy (on compare snd)

    -- for debug
    tapLog a =
      let sideStr = if side == Dark then "Dark" else "Light"
          showScore = intercalate " , "
                  <<< map \(Tuple pos score) → show pos <> ": " <> show score
      in  a <$ (log $ sideStr <> ": " <> showScore a)

    tapLogSafe a = do
      let board' = fromMaybe board $ move board side =<< a
          safeNum = length $ filter identity $ getSafePlaces board' side
      a <$ (log $ "Safe: " <> show safeNum)
