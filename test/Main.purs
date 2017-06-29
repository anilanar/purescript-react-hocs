module Test.Main where

import ReactHocs

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML.Types (htmlElementToElement)
import DOM.Node.Element (id) as Element
import DOM.Node.Types (ElementId(..))
import Data.Either (Either(Right, Left))
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype, unwrap)
import Enzyme.Mount (mount)
import Enzyme.ReactWrapper as E
import Prelude (Unit, bind, discard, flip, pure, unit, ($), (<$>), (<<<), (<>), (==), (>>=))
import React (ReactClass, createClass, createElement, getChildren, getProps, spec)
import React.DOM as R
import React.DOM.Props as RP
import ReactHocs.Class (class WithContextProps)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Karma (runKarma)
import Type.Proxy (Proxy(..))
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

main :: forall eff. Eff (console :: CONSOLE, avar :: AVAR, dom :: DOM | eff) Unit
main = runKarma do
  suite "context" do
    test "getContext"
      let
        wrapper = mount $ createElement messageListWithContext { messages: ["Hello World!"] } []
        btn = E.findReactClass wrapper button
        btnProps = E.props btn

        coerceProps :: Foreign -> { color :: String }
        coerceProps = unsafeCoerce
      in do
        let color = _.color <<< coerceProps $ btnProps
        assert ("wrong context has been passed: " <> color) (color == "#a0a0a0")

    test "readContext"
      let
        child :: ReactClass Unit
        child = accessContext $ createClass $ (spec unit renderChild) { displayName = "Child" }
        renderChild this = do
          ctx <- readContext (Proxy :: Proxy String) this
          pure $ R.div [ RP.className "child", RP._data { ctx } ] [ R.text ctx ]

        parent :: ReactClass Unit
        parent = (flip withContext) "test-string" $ createClass $ (spec unit renderParent) { displayName = "Parent" }
        renderParent this = do
          pure $ createElement child unit []

        wrapper = flip E.find ".child" $ mount $ createElement parent unit []

        readProps :: Foreign -> F { ctx :: String }
        readProps value = do
          ctx <- value ! "data-ctx" >>= readString
          pure { ctx }

      in do
        case runExcept $ readProps $ E.props wrapper of
          Left _ -> failure "ups..."
          Right { ctx } -> assert ("wrong ctx " <> ctx)  $ ctx == "test-string"

  suite "cmapProps" do
    test "cmapProps"
      let
        helloCls :: ReactClass { text :: String }
        helloCls = createClass $ (spec unit renderFn) { displayName = "HelloCls" }

        renderFn this = do
          text <- getProps this >>= pure <<< _.text
          pure $ R.div [ RP.className "msg", RP._data { msg: text } ] [ R.text text ]

        f :: { msg :: String } -> { text :: String }
        f text = { text: text.msg }

        cls :: ReactClass { msg :: String }
        cls = setDisplayName "HelloClsMapped" $ cmapProps f helloCls

        wrapper = flip E.find ".msg" $ mount $ createElement cls { msg: "Hello World!" } []

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

  suite "refs" do
    test "ref"
      let
        cls :: ReactClass Unit
        cls = createClass $ (spec unit renderFn) { displayName = "Ref" }

        renderFn this = do
          pure $ R.div [ ref "div", RP._id "ref" ] []

        wrp = mount $ createElement cls unit []

        element = E.getDOMNode $ flip E.ref "div" $ wrp
      in do
        ElementId eid <- liftEff $ Element.id (htmlElementToElement element)
        assert ("wrong element got: " <> eid <> "\n" <> E.debug wrp) $ eid == "ref"

    test "readRef"
      let
        cls :: ReactClass Unit
        cls = createClass $ (spec unit renderFn) { displayName = "Ref" }

        renderFn this = do
          pure $ R.div [ ref "div", RP._id "ref" ] []

        wrp = mount $ createElement cls unit []

        refs :: Foreign
        refs = toForeign $ (unsafeCoerce (E.instance_ wrp)).refs

      in do
        fnode <- liftEff (unsafeCoerceEff $ readRef "div" refs)
        case runExcept fnode of
          Left _ -> failure ("reference div not found\n" <> E.debug wrp)
          Right node -> do
            ElementId eid <- liftEff $ Element.id (unsafeCoerce node)
            assert ("wrong element got: " <> eid <> "\n" <> E.debug wrp) $ eid == "ref"
