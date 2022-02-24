module DotLang where

import Prelude
import Data.Maybe (Maybe)
import Data.Variant (Variant)

type DotLang
  = { strict :: Boolean
    , type :: Variant ( graph :: Unit, digraph :: Unit )
    , id :: Maybe Id
    , statements :: Array Statement
    }

type Statement
  = Variant
      ( nodeStatement :: NodeStatement
      , edgeStatement :: EdgeStatement
      , attrStatement :: AttrStatement
      , subGraph :: SubGraph
      )

data SubGraph
  = SubGraph { id :: Id, statements :: Array Statement }

data AttrStatement
  = AttrStatement
    ( Variant
        ( graph :: Unit
        , node :: Unit
        , edge :: Unit
        )
    )
    (Array Attr)

data Attr
  = Attr String (Array String)

type Id
  = String

data NodeId
  = NodeId Id (Maybe Port)

data Port
  = Port Id (Maybe CompassPt)

type CompassPt
  = Variant
      ( n :: Unit
      , ne :: Unit
      , e :: Unit
      , se :: Unit
      , s :: Unit
      , sw :: Unit
      , w :: Unit
      , nw :: Unit
      , c :: Unit
      , "_" :: Unit
      )

data NodeStatement
  = NodeStatement NodeId (Array Attr)

type EdgeEnd
  = Variant
      ( nodeId :: NodeId
      , subgraph :: SubGraph
      )

data EdgeStatement
  = EdgeStatement EdgeEnd EdgeEnd (Array EdgeEnd) (Array Attr)
