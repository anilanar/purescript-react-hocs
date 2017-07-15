module ReactHocs.InOutTransition
  ( inOutTransition
  , inOutTransitionSpec
  , InOutState(..)
  , InOutProps(..)
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (RequestAnimationFrameId, cancelAnimationFrame, requestAnimationFrame)
import Data.Array (catMaybes)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over)
import Prelude (Unit, bind, not, pure, unit, ($), (<<<), (>>=))
import React (Event, ReactClass, ReactProps, ReactRefs, ReactSpec, ReactState, ReadOnly, ReadWrite, createClass, getChildren, getProps, readState, spec', transformState)
import React.DOM (div)
import React.DOM.Props (className) as P
import React.DOM.Props (onAnimationEnd, onTransitionEnd)

newtype InOutState = InOutState
  { mounting :: Boolean
  , mounted :: Boolean
  , unmounting :: Boolean
  , frameId :: Maybe RequestAnimationFrameId
  }

derive instance newtypeState :: Newtype InOutState _

newtype InOutProps eff = InOutProps
  -- | pass `false` to animate when the component mounts
  -- | you can switch mounted and the classNameIn and classNameOut get applied
  { mounted :: Boolean
  , onUnmount :: Event -> Eff eff Unit
  , className :: String
  -- | classNameIn is applied only when mounting the component
  -- | it is first applied using `requestAnimationFrame` so that the initial
  -- | state of the commponent can be described by the `className` css class.
  , classNameIn :: String
  -- | applied when the component receives `mounted` property set to `false`.
  , classNameOut :: String
  -- | className that is applied after the animatin in ends, and is removed
  -- | when unmounting animationstarts
  , classNameMounted :: String
  }

derive instance newtypeInOutProps :: Newtype (InOutProps e) _

inOutTransitionSpec
  :: forall e eff
   . ReactSpec
      (InOutProps (props :: ReactProps, state :: ReactState ReadWrite, refs :: ReactRefs ReadOnly | e))
      InOutState
      (dom :: DOM | eff)
inOutTransitionSpec = (spec' getInitialState render)
    { displayName = "InOutTransition"
    , componentWillReceiveProps = componentWillReceiveProps
    , componentWillUnmount = componentWillUnmount
    }
  where
    classNames :: Array (Maybe String) -> String
    classNames = intercalate " " <<< catMaybes

    coerce :: forall e1 a r. Eff (state :: ReactState a | e1) r -> Eff e1 r
    coerce = unsafeCoerceEff

    getInitialState this = do
      InOutProps { mounted } <- getProps this
      frameId <- window >>= requestAnimationFrame 
        (coerce $ transformState this (over InOutState (_ { mounting = true })))
      pure (InOutState
        { mounting: not mounted
        , unmounting: false
        , mounted: mounted
        , frameId: Just frameId
        })

    componentWillUnmount this = do
      InOutState { frameId } <- readState this
      maybe (pure unit) (\fid -> window >>= cancelAnimationFrame fid) frameId

    componentWillReceiveProps this (InOutProps { mounted }) = do
      InOutProps { mounted: mntd } <- getProps this
      transformState this (over InOutState (_ { mounting = mounted, unmounting = not mounted, mounted = false }))

    onEnd this ev = do
      InOutState { unmounting } <- readState this
      if unmounting
        then do
          _ <- transformState this (over InOutState (_ { mounted = false, unmounting = false }))
          InOutProps { onUnmount } <- getProps this
          onUnmount ev
        else
          transformState this (over InOutState (_ { mounting = false, mounted = true }))

    render this = do
      InOutProps { className, classNameIn, classNameOut, classNameMounted } <- getProps this
      InOutState { mounting, unmounting, mounted } <- readState this
      children <- getChildren this
      let class_ = classNames
            [ Just className
            , if mounting then Just classNameIn else Nothing
            , if unmounting then Just classNameOut else Nothing
            , if mounted then Just classNameMounted else Nothing
            ]
      pure $ div
        [ P.className class_
        , onTransitionEnd (onEnd this)
        , onAnimationEnd (onEnd this)
        ]
        children

inOutTransition
  :: forall e
   . ReactClass (InOutProps (props :: ReactProps, state :: ReactState ReadWrite, refs :: ReactRefs ReadOnly | e))
inOutTransition = createClass inOutTransitionSpec
