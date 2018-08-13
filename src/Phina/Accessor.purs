module Phina.Accessor
  ( getWidth
  , getFps
  , getDeltaTime
  , setFill
  , setAlpha
  , setBackgroundColor
  , setText
  , setVisible
  ) where


import Effect (Effect)
import Phina (class ReadableProp, class WritableProp, Color, Duration, getProp, setProp)
import Type.Prelude (SProxy(..))


getWidth ∷ ∀ a. ReadableProp a "width" Number ⇒ a → Effect Number
getWidth = getProp (SProxy ∷ SProxy "width")

getFps ∷ ∀ a. ReadableProp a "fps" Number ⇒ a → Effect Number
getFps = getProp (SProxy ∷ SProxy "fps")

getDeltaTime ∷ ∀ a. ReadableProp a "deltaTime" Duration ⇒ a → Effect Duration
getDeltaTime = getProp (SProxy ∷ SProxy "deltaTime")

setFill ∷ ∀ a. WritableProp a "fill" Color ⇒ Color → a → Effect a
setFill = setProp (SProxy ∷ SProxy "fill")

setAlpha ∷ ∀ a. WritableProp a "alpha" Number ⇒ Number → a → Effect a
setAlpha = setProp (SProxy ∷ SProxy "alpha")

setBackgroundColor
   ∷ ∀ a
   . WritableProp a "backgroundColor" Color
  ⇒ Color
  → a
  → Effect a
setBackgroundColor = setProp (SProxy ∷ SProxy "backgroundColor")

setText ∷ ∀ a. WritableProp a "text" String ⇒ String → a → Effect a
setText = setProp (SProxy ∷ SProxy "text")

setVisible ∷ ∀ a. WritableProp a "visible" Boolean ⇒ Boolean → a → Effect a
setVisible = setProp (SProxy ∷ SProxy "visible")
