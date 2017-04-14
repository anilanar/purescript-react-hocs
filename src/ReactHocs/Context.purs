module ReactHocs.Context
  ( withContext
  , getFromContext
  , getContext
  ) where

import Prelude (id, flip, const, ($))
import React (ReactClass)
import Data.Lens (Lens', view, lens)

import ReactHocs.Class (class WithContextProps, setCtx)

foreign import withContext
  :: forall props ctx
   . ReactClass props
  -> ctx
  -> ReactClass props

foreign import getFromContext_
  :: forall props props' ctx ctx'
   . (ctx' -> props' -> props)
  -> (ctx -> ctx')
  -> ReactClass props
  -> ReactClass props'

getFromContext
  :: forall props props' ctx ctx'
   . (WithContextProps props' props ctx')
  => Lens' ctx ctx'
  -> ReactClass props
  -> ReactClass props'
getFromContext _lens cls = getFromContext_ setCtx_ (view _lens) cls
  where
    setCtx_ :: ctx' -> props' -> props
    setCtx_ = setCtx

getContext
  :: forall props props' ctx
   . (WithContextProps props' props ctx)
  => ReactClass props
  -> ReactClass props'
getContext = getFromContext _id
  where
    _id :: Lens' ctx ctx
    _id = lens id (flip $ const id)
