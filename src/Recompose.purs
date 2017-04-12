module Recompose
  ( module Context
  , module Contravariant
  , setDisplayName
  ) where

import React (ReactClass)
import Recompose.Contravariant as Contravariant
import Recompose.Context as Context

foreign import setDisplayName
  :: forall props
   . String
  -> ReactClass props
  -> ReactClass props
