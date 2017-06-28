module ReactHocs.Refs 
  ( ref
  , readRef
  ) where

import DOM.Node.Types (Node, readNode)
import Data.Foreign (F, Foreign, toForeign)
import Data.Foreign.Index (readProp)
import Prelude (join, ($), (<$>))
import React (Refs)
import React.DOM.Props (Props, unsafeMkProps)

ref :: String -> Props
ref = unsafeMkProps "ref"

readRef :: String -> Foreign -> F Node
readRef name refs = join $ readNode <$> prop
  where
    prop :: F Foreign
    prop = readProp name (toForeign refs)
