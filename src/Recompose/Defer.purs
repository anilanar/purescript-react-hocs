module Recompose.Defer 
  ( deferByAnimationFrame
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import React as R
import React (ReactClass, ReactSpec, ReactElement)

import Recompose.DisplayName (getDisplayName)

foreign import nullElement :: ReactElement

foreign import requestAnimationFrame :: forall eff. Eff eff Unit -> Eff eff Unit

-- | Insipred by [DeferRender](https://gist.github.com/sieverk/b8815bf0b64e1fc0ef0aac298b7298f4#file-defercomponentrender-js)
-- | Useful for rendering heavy components to allow the browser finish
-- | animation.  This might help with percieving time of rendering.  Check the original
-- | [blog post](https://medium.com/@paularmstrong/twitter-lite-and-high-performance-react-progressive-web-apps-at-scale-d28a00e780a3#983c).
deferByAnimationFrame
  :: forall eff props
  .  Int
  -> ReactClass props
  -> ReactSpec props { count :: Int } eff
deferByAnimationFrame count cls = (R.spec { count } renderFn)
    { displayName = getDisplayName cls <> "Defer"
    , componentDidMount = componentDidMount
    , shouldComponentUpdate = shouldComponentUpdate
    }
  where
    shouldComponentUpdate this props state = pure $ state.count == 0

    componentDidMount this = do
      requestAnimationFrame (frmEff this)

    frmEff this = do
      n <- R.readState this >>= (pure <<< _.count)
      R.transformState this (_ { count = n - 1 })
      if count > 0
        then requestAnimationFrame (frmEff this)
        else pure unit


    renderFn this = do
      props <- R.getProps this
      n <- R.readState this >>= (pure <<< _.count)
      if n > 0
        then pure nullElement
        else pure $ R.createElement cls props []
