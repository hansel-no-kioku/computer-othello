module Data.Primitivizable
  ( class Primitivizable
  , primitivize
  , evolve
  , class PrimitivizableRecord
  , primitivizeRecord
  , evolveRecord
  ) where


import Prelude

import Data.Primitive (class Primitive)
import Data.Symbol (class IsSymbol)
import Prim.Row as R
import Prim.RowList as RL
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Prelude (class ListToRow, RLProxy(..), SProxy(..), reflectSymbol)


class Primitive b ⇐ Primitivizable a b | a → b where
  primitivize ∷ a → b
  evolve ∷ b → a

instance primitivizableBoolean ∷ Primitivizable Boolean Boolean where
  primitivize a = a
  evolve a = a

instance primitivizableInt ∷ Primitivizable Int Int where
  primitivize a = a
  evolve a = a

instance primitivizableNumber ∷ Primitivizable Number Number where
  primitivize a = a
  evolve a = a

instance primitivizableString ∷ Primitivizable String String where
  primitivize a = a
  evolve a = a

instance primitivizableArray ∷ Primitivizable a b
                                  ⇒ Primitivizable (Array a) (Array b) where
  primitivize a = primitivize <$> a
  evolve a = evolve <$> a

class (ListToRow rl sr, ListToRow rlp srp)
  ⇐ PrimitivizableRecord rl rlp r rp sr srp
  | rl → rlp, rlp → rl, r → rp, rp → r, rl → sr, rlp → srp
  where
    primitivizeRecord ∷ RLProxy rl → Record r → Record srp
    evolveRecord ∷ RLProxy rlp → Record rp → Record sr

instance primitivizableRecordNil ∷ PrimitivizableRecord RL.Nil RL.Nil r rp () ()
  where
    primitivizeRecord _ _ = {}
    evolveRecord _ _ = {}

instance primitivizableRecordCons ∷
  ( IsSymbol s
  , Primitivizable ty typ
  , PrimitivizableRecord tailRl tailRlP r rp sTailR sTailRP
  , ListToRow tailRl sTailR
  , ListToRow tailRlP sTailRP
  , R.Cons s ty sTailR tailR
  , R.Cons s typ sTailRP tailRP
  ) ⇒ PrimitivizableRecord (RL.Cons s ty tailRl) (RL.Cons s typ tailRlP) r rp tailR tailRP
  where
    primitivizeRecord _ r = insert (primitivize $ get r) tail
      where
        key = reflectSymbol (SProxy ∷ SProxy s)
        get = unsafeGet key ∷ Record r → ty
        insert = unsafeSet key ∷ typ → Record sTailRP → Record tailRP
        tail = primitivizeRecord (RLProxy ∷ RLProxy tailRl) r
    evolveRecord _ r = insert (evolve $ get r) tail
      where
        key = reflectSymbol (SProxy ∷ SProxy s)
        get = unsafeGet key ∷ Record rp → typ
        insert = unsafeSet key ∷ ty → Record sTailR → Record tailR
        tail = evolveRecord (RLProxy ∷ RLProxy tailRlP) r

instance primitivizableRecord ∷
  ( RL.RowToList r rl
  , RL.RowToList rp rlp
  , Primitive (Record rp)
  , PrimitivizableRecord rl rlp r rp r rp
  ) ⇒ Primitivizable (Record r) (Record rp)
  where
    primitivize = primitivizeRecord (RLProxy ∷ RLProxy rl)
    evolve = evolveRecord (RLProxy ∷ RLProxy rlp)
