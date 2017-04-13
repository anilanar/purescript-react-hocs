module ReactHocs.Context
  ( withContext
  , getFromContext
  , getContext
  ) where

import React (ReactClass)

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
  => (ctx -> ctx')
  -> ReactClass props
  -> ReactClass props'
getFromContext f cls = getFromContext_ setCtx_ f cls
  where
    setCtx_ :: ctx' -> props' -> props
    setCtx_ = setCtx

getContext
  :: forall props props' ctx
   . (WithContextProps props' props ctx)
  => ReactClass props
  -> ReactClass props'
getContext = getFromContext id_
  where
    id_ :: ctx -> ctx
    id_ x = x
