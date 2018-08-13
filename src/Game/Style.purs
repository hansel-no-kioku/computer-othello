module Game.Style
  ( layout
  , style
  ) where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import Game.Config (config)
import Game.Othello.Board (Side(..), enemy)
import Phina (CircleShape, Color, DisplayScene, Label, Position, Tween, addTween, call, color, easeInOutSine, easeInSine, easeOutElastic, easeOutInCubic, easeOutSine, getSpanPos, msec, nullColor, play, remove, scaleTo, sec, set, setLoop, setProps, setText, stop, to, toParams, wait)
import Phina.Accessor (setVisible)


type Layout = DisplayScene → Effect Position

layout =
  { titleScene:
      { title: getSpanPos 8 3 ∷ Layout
      , settingPanel: getSpanPos 8 9 ∷ Layout
      , startButton: getSpanPos 8 13 ∷ Layout
      }
  , mainScene:
      { nameLabel: case _ of
          Dark → getSpanPos 4 1 ∷ Layout
          Light → getSpanPos 12 1 ∷ Layout
      , scoreDigit: \side digit →
          let digitWidth = 44.0
          in  map (_ + {x: digitWidth * (0.5 - toNumber digit), y: 0.0})
                <<< case side of
                  Dark → getSpanPos 4 2 ∷ Layout
                  Light → getSpanPos 12 2 ∷ Layout
      , board: getSpanPos 8 9 ∷ Layout
      }
  }

style =
  { titleScene:
      { backgroundColor: color "#393"
      }

  , title:
      { text: config.title
      , fontSize: 98.0
      , fontWeight: "bold"
      , fill: color "white"
      , stroke: color "black"
      , strokeWidth: 4.0
      }

  , settingPanel:
      { width: 520.0
      , height: 280.0
      }

  , selectButton: \{width, height} →
      { width: width - 16.0
      , height: height - 8.0
      , cornerRadius: (height - 8.0) / 2.0
      , stroke: color "grey"
      , strokeWidth: 4.0
      , fontSize: 24.0
      }

  , selectButtonOn: case _ of
      Dark →
        { fill: pieceColor Dark
        , fontColor: pieceColor Light
        }
      Light →
        { fill: pieceColor Light
        , fontColor: pieceColor Dark
        }

  , selectButtonOff:
      { fill: color "#666"
      , fontColor: color "#aaa"
      }

  , startButton:
      { width: 320.0
      , height: 72.0
      , cornerRadius: 36.0
      , fill: color "#37d"
      , stroke: color "white"
      , strokeWidth: 6.0
      , text: "Game Start"
      , fontColor: color "white"
      , fontSize: 40.0
      }
  , startButtonPushed:
      { fill: color "#7bf"
      , stroke: nullColor
      , shadow: color "#7bf"
      , shadowBlur: 8.0
      }

  , bgPiecesStage:
      { fill: color "transparent"
      , padding: 0.0
      , stroke: nullColor
      , alpha: 0.9
      }

  , bgPiece:
      { width: 80.0
      , num: 16
      , speed: 320.0
      , splashSpeed: 1200.0
      }
  , bgPieceAnimation

  , stage:
      { fill: color "#e72"
      , padding: 0.0
      , originX: 0.0
      , originY: 0.0
      , stroke: nullColor
      , alpha: 0.0
      }

  , board: \width →
      let
        boardWidth = width - 24.0 * 2.0
      in
        { width: boardWidth
        , height: boardWidth
        , padding: 16.0
        , fill: color "#393"
        , stroke: color "#333"
        , strokeWidth: 4.0
        , shadow: color "black"
        , shadowBlur: 16.0
        }
  , cell: \width →
      let
        strokeWidth = 2.0
        cellWidth = width - strokeWidth
      in
        { width: cellWidth
        , height: cellWidth
        , fill: color "transparent"
        , padding: strokeWidth / 2.0
        , stroke: color "#444"
        , strokeWidth
        }
  , piece: \width side →
      { radius: width / 2.0 - 8.0
      , fill: pieceColor side
      , stroke: color "#999"
      , strokeWidth: 4.0
      , shadow: color "black"
      , shadowBlur: 8.0
      }
  , pieceColor

  , nameLabel: \side name →
      { text: name
      , fill: pieceColor side
      , fontSize: 30.0
      , fontWeight: "bold"
      , stroke: color "grey"
      , strokeWidth: 2.0
      }
  , turnAnimation

  , digit: \side digit →
      { fill: pieceColor side
      , fontSize: 52.0
      , fontWeight: "bold"
      , stroke: color "grey"
      , strokeWidth: 2.0
      }
  , digitAnimation

  , suggestMask: \width →
      { radius: width / 2.0 - 4.0
      , fill: color "#393"
      , stroke: nullColor
      }
  , suggestAnimation

  , movableIconAlpha: 0.25
  , flipAnimation

  , passLabel:
      { text: "Pass !"
      , fill: color "white"
      , fontSize: 72.0
      , fontWeight: "bold"
      , stroke: color "black"
      , strokeWidth: 6.0
      , padding: 24.0
      , shadow: color "black"
      , shadowBlur: 24.0
      , visible: false
      }
  , passAnimation

  , finishLabel:
      { text: "Finish !"
      , fill: color "white"
      , fontSize: 120.0
      , fontWeight: "bold"
      , stroke: color "black"
      , strokeWidth: 8.0
      , padding: 32.0
      , shadow: color "black"
      , shadowBlur: 32.0
      , visible: false
      }
  , finishAnimation

  , sceneTransitionTime: 0.5

  , resultSceneAlpha: 0.7
  }


pieceColor ∷ Side → Color
pieceColor Dark = color "#222"
pieceColor Light = color "#ddd"


bgPieceAnimation ∷ Number → Tween CircleShape
bgPieceAnimation fluctuation = do
  let duration = msec $ 250.0 * fluctuation
  setLoop true
  to (toParams {scaleX: 0.0}) duration easeInSine
  set (toParams {fill: pieceColor Light})
  to (toParams {scaleX: 1.0}) duration easeOutSine
  to (toParams {scaleX: 0.0}) duration easeInSine
  set (toParams {fill: pieceColor Dark})
  to (toParams {scaleX: 1.0}) duration easeOutSine


suggestAnimation
   ∷ Side
  → CircleShape
  → Effect {play ∷ Effect Unit, stop ∷ Effect Unit}
suggestAnimation side icon = ado
  tweener ← addTween animation icon
  in  { play: void $ play tweener
      , stop: stop tweener
              *> setProps {scaleX: 1.0, fill: pieceColor $ enemy side} icon
              $> unit
      }

  where
    animation = do
      setLoop true
      wait (msec 250)
      to (toParams {scaleX: 0.0}) (msec 250) easeInSine
      set (toParams {fill: pieceColor side})
      to (toParams {scaleX: 1.0}) (msec 250) easeOutSine
      to (toParams {scaleX: 0.0}) (msec 250) easeInSine
      set (toParams {fill: pieceColor $ enemy side})
      to (toParams {scaleX: 1.0}) (msec 250) easeOutSine
      wait (msec 250)


turnAnimation ∷ Label → Effect {play ∷ Effect Unit, stop ∷ Effect Unit}
turnAnimation label = ado
  tweener ← addTween animation label
  in  { play: void $ play tweener
      , stop: stop tweener *> setProps {scaleX: 1.0} label $> unit
      }

  where
    animation = do
      setLoop true
      to (toParams {scaleX: -1.0}) (msec 500) easeInOutSine
      to (toParams {scaleX: 1.0}) (msec 500) easeInOutSine


flipAnimation ∷ Number → Side → Effect Unit → Tween CircleShape
flipAnimation dist side f = do
  wait $ msec $ dist * 250.0
  to (toParams {scaleX: 0.0}) (msec 250) easeInSine
  set (toParams {fill: pieceColor side})
  to (toParams {scaleX: 1.0}) (msec 250) easeOutSine
  call \a → f $> a


digitAnimation ∷ String → Tween Label
digitAnimation text = do
  to (toParams {scaleY: 0.0}) (msec 250) easeInSine
  call $ setText text
  to (toParams {scaleY: 1.0}) (msec 250) easeOutSine


passAnimation ∷ Number → Effect Unit → Tween Label
passAnimation width f = do
  let margin = 200.0
  set (toParams {x: width + margin})
  call $ setVisible true
  to (toParams {x: -margin}) (sec 2) easeOutInCubic
  call \a → remove a *> f $> a


finishAnimation ∷ Effect Unit → Tween Label
finishAnimation f = do
  let initScale = 0.5
  set (toParams {scaleX: initScale, scaleY: 0.0})
  call $ setVisible true
  to (toParams {scaleY: initScale}) (msec 100) easeOutSine
  to (toParams {scaleY: -initScale}) (msec 200) easeInOutSine
  to (toParams {scaleY: initScale}) (msec 200) easeInOutSine
  to (toParams {scaleY: -initScale}) (msec 200) easeInOutSine
  to (toParams {scaleY: initScale}) (msec 200) easeInOutSine
  to (toParams {scaleY: -initScale}) (msec 200) easeInOutSine
  to (toParams {scaleY: initScale}) (msec 200) easeInOutSine
  to (toParams {scaleY: -initScale}) (msec 200) easeInOutSine
  to (toParams {scaleY: initScale}) (msec 200) easeInOutSine
  scaleTo 1.0 (sec 1) easeOutElastic
  wait (sec 1)
  call \a → remove a *> f $> a
