module Test.Main where

import Prelude
import Morph
import Enzyme.ReactWrapper as E
import React.DOM as R
import React.DOM.Props as RP
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Array as A
import Data.Foreign (Foreign, toForeign)
import Data.Newtype (class Newtype, unwrap)
import Enzyme.Mount (mount)
import Morph.Class (class WithContextProps)
import React (ReactClass, createClass, createElement, getChildren, getProps, spec)
import Test.Unit (success, suite, test, timeout)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Karma (runKarma)
import Unsafe.Coerce (unsafeCoerce)

newtype ButtonProps = ButtonProps { color :: String }

derive instance newtypeButtonProps :: Newtype ButtonProps _

instance withContextPropsButtonProps :: WithContextProps Unit ButtonProps String where
  setCtx color _ = ButtonProps { color }

button :: ReactClass ButtonProps
button = createClass $ (spec unit renderFn) { displayName = "Button" }
  where
    renderFn this = do
      color <- (getProps this) >>= (pure <<< _.color <<< unwrap)
      children <- getChildren this
      pure $ R.button [ RP.className "btn", RP.style { background: color } ] children

buttonWithContext :: ReactClass Unit
buttonWithContext = getFromContext id_ button
  where
    id_ :: String -> String
    id_ s = s

message :: ReactClass { text :: String }
message = createClass $ (spec unit renderFn) { displayName = "Message" }
  where
    renderFn this = do
      text <- getProps this >>= (pure <<< _.text)
      pure $ R.div [ RP.className "msg" ] [ R.text text, createElement buttonWithContext unit [] ]

messageList :: ReactClass { messages :: Array String }
messageList = createClass $ (spec unit renderFn) { displayName = "MessageList" }
  where
    renderFn this = do
      messages <- getProps this >>= (pure <<< _.messages)
      pure $ R.div [ RP.className "msg-list" ] (toMessage <$> messages)

    toMessage text = createElement message { text } []

messageListWithContext :: ReactClass { messages :: Array String }
messageListWithContext = withContext messageList "#a0a0a0"

main = runKarma do
  suite "context" do
    test "withContext and getContext"
      let
        wrapper = mount (createElement messageListWithContext { messages: ["Hello World!"] } []) (toForeign {})
        btn = E.findReactClass wrapper button
        btnProps = E.props btn

        coerceProps :: Foreign -> { color :: String }
        coerceProps = unsafeCoerce
      in do
        let color = _.color <<< coerceProps $ btnProps
        assert ("wrong context has been passed: " <> color) (color == "#a0a0a0")
