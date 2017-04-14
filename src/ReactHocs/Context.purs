module ReactHocs.Context
  ( withContext
  , getFromContext
  , getContext
  , accessContext
  , readContext
  , CONTEXT
  ) where

import Prelude (id, flip, const, ($))
import Control.Monad.Eff (kind Effect, Eff)
import Data.Lens (Lens', view, lens)
import React (ReactClass, ReactThis)
import Type.Proxy (Proxy)

import ReactHocs.Class (class WithContextProps, setCtx)

foreign import data CONTEXT :: Effect

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
  -- proxy is needed to resolve WithContextProps
  => Proxy ctx
  -> ReactClass props
  -> ReactClass props'
getContext p = getFromContext _id
  where
    _id :: Lens' ctx ctx
    _id = lens id (flip $ const id)

-- | This function mutates the component by adding `contextTypes` property.
foreign import accessContext
  :: forall props
   . ReactClass props
  -> ReactClass props

-- | You can use it with components that were passed through `accessContext`.
foreign import readContext
  :: forall props state eff ctx
   . Proxy ctx
  -> ReactThis props state
  -> Eff (context :: CONTEXT | eff) ctx
