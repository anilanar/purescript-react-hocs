module Recompose.Contravariant
  ( cmapProps
  , cmapPropsSpec
  ) where

import Prelude
import React as R
import React (ReactClass, ReactElement, ReactSpec)
import Recompose.DisplayName (getDisplayName, mapDisplayName)

-- | Turn `ReactClass` into contravariant functor
-- | It returns a React stateless component
cmapProps
  :: forall props props'
   . (props' -> props)
  -> ReactClass props
  -> ReactClass props'
cmapProps f cls = mapDisplayName (_ <> "Mapped") $ R.createClassStateless renderFn
  where
    renderFn :: props' -> ReactElement
    renderFn props' = R.createElement cls (f props') []

-- | You can turn `ReactSpec` into contravariant functor with this fuction.
-- | It returns a `ReactSpec`, i.e. it will be rendered as statefull react
-- | component - unlike `cmapProps`.
cmapPropsSpec
  :: forall props props' eff
   . (props' -> props)
  -> ReactClass props
  -> ReactSpec props' Unit eff
cmapPropsSpec f cls = (R.spec unit renderFn) { displayName = getDisplayName cls <> "Mapped" }
  where
    renderFn this = do
      props <- R.getProps this
      pure $ R.createElement cls (f props) []
