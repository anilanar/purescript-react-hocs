module ReactHocs.Refs 
  ( ref
  , readRef
  ) where

import Control.Monad.Eff (Eff)
import DOM.Node.Types (Node, readNode)
import Data.Foreign (F, Foreign, toForeign)
import Data.Foreign.Index (readProp)
import Prelude (join, pure, ($), (<$>))
import React (ReactRefs, Read)
import React.DOM.Props (Props, unsafeMkProps)

ref :: String -> Props
ref = unsafeMkProps "ref"

readRef :: forall access eff. String -> Foreign ->  Eff (refs :: ReactRefs (read :: Read | access) | eff) (F Node) 
readRef name refs = pure $ join $ readNode <$> readProp name (toForeign refs)
