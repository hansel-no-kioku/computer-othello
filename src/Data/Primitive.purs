module Data.Primitive
  ( class Primitive
  , class PrimitiveRecord
  ) where

import Data.Symbol (class IsSymbol)
import Prim.RowList as RL
import Type.Prelude (class ListToRow)


class Primitive a

instance primitiveBoolean ∷ Primitive Boolean

instance primitiveInt ∷ Primitive Int

instance primitiveNumber ∷ Primitive Number

instance primitiveString ∷ Primitive String

instance primitiveArray ∷ Primitive a ⇒ Primitive (Array a)

class PrimitiveRecord (rl ∷ RL.RowList) (r ∷ # Type) | rl → r

instance primitiveRecordNil ∷ PrimitiveRecord RL.Nil ()

instance primitiveRecordCons ∷
  ( IsSymbol s
  , Primitive ty
  , PrimitiveRecord tailRl tailR
  , ListToRow (RL.Cons s ty tailRl) r
  ) ⇒ PrimitiveRecord (RL.Cons s ty tailRl) r

instance primitiveRecord ∷
  ( RL.RowToList r rl
  , ListToRow rl r
  , PrimitiveRecord rl r
  ) ⇒ Primitive (Record r)
