module Recompose.Contravariant
  ( cmapProps
  , cmapPropsSpec
  ) where

import Prelude
import React as R
import React (ReactClass, ReactElement, ReactSpec)

-- |
-- Turn `ReactClass` into contravariant functor
cmapProps
  :: forall props props'
   . (props' -> props)
  -> ReactClass props
  -> ReactClass props'
cmapProps f cls = R.createClassStateless renderFn
  where
    renderFn :: props' -> ReactElement
    renderFn props' = R.createElement cls (f props') []

-- |
-- Turn `ReactSpec` into contravariant functor
cmapPropsSpec
  :: forall props props' state eff
   . (props' -> props)
  -> ReactSpec props state eff
  -> ReactSpec props' Unit eff
cmapPropsSpec f sp = (R.spec unit renderFn) { displayName = sp.displayName <> "Mapped" }
  where
    cls = R.createClass sp
    renderFn this = do
      props <- R.getProps this
      pure $ R.createElement cls (f props) []
