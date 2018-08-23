module Game.View.TurnAnimation
  ( TurnAnimation
  , newTurnAnimation
  , setActive
  ) where


import Prelude

import Effect (Effect)
import Effect.Ref (Ref, modify, new, read)
import Phina (Label, Tween, Tweener, addTween, newLabel, pause, resume)


type TurnAnimation = Ref
  { tweener ∷ Tweener Label
  , isActive ∷ Boolean
  }


emptyTurnAnimation ∷ Effect TurnAnimation
emptyTurnAnimation = do
  dummyLabel ← newLabel {}
  dummyTweener ← addTween (pure unit) dummyLabel
  new {tweener: dummyTweener, isActive: false}


newTurnAnimation
   ∷ Label
  → ((Label → Effect Label) → Tween Label)
  → Effect TurnAnimation
newTurnAnimation label animation = do
  dummyTweener ← addTween (pure unit) label
  turnAnimation ← new {tweener: dummyTweener, isActive: false}

  let tween = animation $ controller turnAnimation
  tweener ← addTween tween label
  _ ← modify (_ {tweener = tweener}) turnAnimation

  pure turnAnimation

  where
    controller turnAnimation l = do
      {tweener, isActive} ← read turnAnimation
      l <$ unless isActive (void $ pause tweener)


setActive ∷ Boolean → TurnAnimation → Effect Unit
setActive isActive turnAnimation = do
  {tweener} ← read turnAnimation
  when isActive (void $ resume tweener)
  void $ modify (_ {isActive = isActive}) turnAnimation
