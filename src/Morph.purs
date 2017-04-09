module Morph
  ( withContext
  , getFromContext
  , getContext
  ) where

import Prelude
import Morph.Class
import Optic.Setter (set)
import React (ReactClass, ReactElement, ReactSpec)
import React as R
import Thermite as T

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

-- |
-- Turn `ReactClass` into contravariant functor
mapProps
  :: forall props props'
   . (props' -> props)
  -> ReactClass props
  -> ReactClass props'
mapProps f cls = R.createClassStateless renderFn
  where
    renderFn :: props' -> ReactElement
    renderFn props' = R.createElement cls (f props') []

-- |
-- Turn `ReactSpec` into contravariant functor
mapPropsSpec
  :: forall props props' state eff
   . (props' -> props)
  -> ReactSpec props state eff
  -> ReactSpec props' Unit eff
mapPropsSpec f sp = R.spec unit renderFn
  where
    cls = R.createClass sp
    renderFn this = do
      props <- R.getProps this
      pure $ R.createElement cls (f props) []

-- Turn `Thermite.Spec` into contravariant functor.
-- Thermite does not expose Spec constructor
-- https://github.com/paf31/purescript-thermite/pull/83
{--
  - mapPropsTSpec
  -   :: forall props props' state action eff
  -    . (props' -> props)
  -   -> T.Spec eff state props action
  -   -> T.Spec eff state props' action
  - mapPropsTSpec f (T.Spec sp) = T.simpleSpec performAction render
  -   where
  -     performAction a p = sp.performAction a (f p)
  -     render a p = sp.render a (f p)
  --}
