module Test.Main where

import Prelude
import ReactHocs
import ReactHocs.Class (class WithContextProps)
import Enzyme.ReactWrapper as E
import React.DOM as R
import React.DOM.Props as RP
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype, unwrap)
import Type.Proxy (Proxy(..))
import Enzyme.Mount (mount)
import React (ReactClass, createClass, createElement, getChildren, getProps, spec)
import Test.Unit (failure, suite, test)
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
buttonWithContext = getContext (Proxy :: Proxy String) button

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

main :: forall eff. Eff (console :: CONSOLE, avar :: AVAR | eff) Unit
main = runKarma do
  suite "context" do
    test "withContext and getContext"
      let
        wrapper = mount $ createElement messageListWithContext { messages: ["Hello World!"] } []
        btn = E.findReactClass wrapper button
        btnProps = E.props btn

        coerceProps :: Foreign -> { color :: String }
        coerceProps = unsafeCoerce
      in do
        let color = _.color <<< coerceProps $ btnProps
        assert ("wrong context has been passed: " <> color) (color == "#a0a0a0")

  suite "cmapProps" do
    test "cmapProps" 
      let
        helloCls :: ReactClass { text :: String }
        helloCls = createClass $ (spec unit renderFn) { displayName = "HelloCls" }

        renderFn this = do
          text <- getProps this >>= (pure <<< _.text)
          pure $ R.div [ RP.className "msg", RP._data { msg: text } ] [ R.text text ]

        f :: { msg :: String } -> { text :: String }
        f text = { text: text.msg }

        cls :: ReactClass { msg :: String }
        cls = setDisplayName "HelloClsMapped" $ cmapProps f helloCls

        wrapper = E.find (mount $ createElement cls { msg: "Hello World!" } []) ".msg"

        readProps :: Foreign -> F { msg :: String }
        readProps value = do
          msg <- value ! "data-msg" >>= readString
          pure { msg }
      in 
        case runExcept $ readProps $ E.props wrapper of
          Left _ -> failure "ups..."
          Right props -> do
            let text = props.msg
            assert ("wrong text prop " <> text) $ text == "Hello World!"
