module ReactHocs.DisplayName
  ( getDisplayName
  , mapDisplayName
  , setDisplayName
  ) where

import React (ReactClass)

foreign import setDisplayName
  :: forall props
   . String
  -> ReactClass props
  -> ReactClass props

foreign import mapDisplayName
  :: forall props
  .  (String -> String)
  -> ReactClass props
  -> ReactClass props

foreign import getDisplayName
  :: forall props
  .  ReactClass props
  -> String
