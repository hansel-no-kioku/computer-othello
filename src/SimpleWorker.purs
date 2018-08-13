module SimpleWorker
  ( class SimpleWorker
  , Worker
  , newWorker
  , request
  , response
  ) where


import Prelude

import Data.Either (Either(..))
import Data.Primitive (class Primitive)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Type.Prelude (Proxy)


class (Primitive args, Primitive result)
              ⇐ SimpleWorker worker args result | worker → args result

foreign import data Worker ∷ Type → Type

newWorker ∷ ∀ w a r. SimpleWorker w a r ⇒ String → Effect (Worker w)
newWorker = runEffectFn1 _newWorker

request ∷ ∀ w a r. SimpleWorker w a r ⇒ a → Worker w → Aff r
request a w = makeAff \f → runEffectFn3 _request a (f <<< Right) w $> nonCanceler

response ∷ ∀ w a r. SimpleWorker w a r ⇒ Proxy w → (a → Effect r) → Effect Unit
response _ = runEffectFn1 _response

foreign import _newWorker ∷ ∀ w. EffectFn1 String w
foreign import _request ∷ ∀ w a r. EffectFn3 a (r → Effect Unit) w Unit
foreign import _response ∷ ∀ a r. EffectFn1 (a → Effect r) Unit
