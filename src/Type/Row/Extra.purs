module Type.Row.Extra where

import Prim.Row (class Cons)
import Type.Data.List (Cons', List', Nil')

--------------------------------------------------------------------------------

class FilterRow :: Row Type -> List' Symbol -> Row Type -> Constraint
class FilterRow r ss r' | ss -> r'

instance filterRowNil :: FilterRow r Nil' ()

instance filterRowCons ::
  ( FilterRow r ss r'
  , Cons s t tr r
  , Cons s t r' r''
  ) =>
  FilterRow r (Cons' s ss) r''

--------------------------------------------------------------------------------