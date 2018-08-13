module Effect.FSM
  ( Machine
  , machine
  , machineAff
  , send
  , receive
  , connect
  , connectWithFilter
  ) where

import Prelude

import Data.Maybe (Maybe, maybe, maybe')
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write)
import Signal as S
import Signal.Aff (mapAff)
import Signal.Channel as SC
import Signal.Effect as SE


data Machine i o = Machine (SC.Channel i) (S.Signal o)


machine
   ∷ ∀ s i o
   . (i → s → Effect (Tuple (Maybe o) s))
  → s
  → i
  → o
  → Effect (Machine i o)
machine f is ii io = do
  ci ← SC.channel ii
  co ← SC.channel io
  let si = SC.subscribe ci
      so = SC.subscribe co
      f' i s = f i s >>= \(Tuple o s') → maybe' pure (SC.send co) o $> s'
  _ ← SE.foldEffect f' is si
  pure $ Machine ci so


-- If the process is interrupted by another input,
-- state consistency is not guaranteed.
machineAff
   ∷ ∀ s i o
   . (i → s → Aff (Tuple (Maybe o) s))
  → s
  → i
  → o
  → Effect (Machine i o)
machineAff f is ii io = do
  ref ← new is
  ci ← SC.channel ii
  let si = SC.subscribe ci
      f' i = do
        s ← liftEffect $ read ref
        Tuple o s' ← f i s
        liftEffect $ write s' ref
        maybe (throwError $ error "no output") pure o
  Machine ci <$> S.filterMap identity io <$> (mapAff f' <@> si)


send ∷ ∀ i o. i → Machine i o → Effect Unit
send i (Machine ci _) = SC.send ci i


receive ∷ ∀ i o. Machine i o → (o → Effect Unit) → Effect Unit
receive (Machine _ so) f = void $ SE.mapEffect f <@> so


connect ∷ ∀ i io o. Machine i io → Machine io o → Effect (Machine i o)
connect (Machine i so) (Machine ci o) = do
  _ ← SE.mapEffect (SC.send ci) <@> so
  pure $ Machine i o


connectWithFilter
   ∷ ∀ i io o
   . (S.Signal io → S.Signal io)
  → Machine i io
  → Machine io o
  → Effect (Machine i o)
connectWithFilter f (Machine i so) (Machine ci o) = do
  _ ← SE.mapEffect (SC.send ci) <@> f so
  pure $ Machine i o
