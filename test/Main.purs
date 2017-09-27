module Test.Main where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Control.Monad.ST (ST, newSTRef, readSTRef, writeSTRef)
import DOM (DOM)
import DOM.HTML.Types (htmlElementToElement)
import DOM.Node.Element (id) as Element
import DOM.Node.Types (ElementId(..))
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foreign (F, Foreign, readBoolean, readString, renderForeignError, toForeign)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype, unwrap)
import Enzyme.Mount (mount)
import Enzyme.ReactWrapper (ReactWrapper)
import Enzyme.ReactWrapper as E
import Enzyme.Types (ENZYME)
import Prelude (Unit, bind, discard, join, pure, show, unit, void, ($), (*>), (<$>), (<<<), (<>), (==), (>>=))
import React (ReactClass, ReactElement, ReactProps, ReactRefs, ReactSpec, ReactState, ReactThis, ReadOnly, createClass, createClassStateless, createElement, getChildren, getProps, readState, spec, writeState)
import React.DOM as R
import React.DOM.Props as RP
import ReactHocs (accessContext, cmapProps, getContext, readContext, readRef, ref, setDisplayName, withContext)
import ReactHocs.Class (class WithContextProps)
import ReactHocs.Context (withContext')
import ReactHocs.IsMounted (isMounted, readIsMounted)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert (assert, equal)
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
messageListWithContext = withContext "#a0a0a0" messageList

colorProvider :: ReactClass { color :: String }
colorProvider = withContext' (\{ color } -> color) $ createClassStateless \_ -> createElement buttonWithContext unit []

main :: forall eff stateRef. Eff (avar :: AVAR,  console :: CONSOLE, dom :: DOM, enzyme :: ENZYME, st :: ST stateRef | eff) Unit
main = runKarma do
  suite "context" do
    test "withContext && getContext" do
      btnProps <- liftEff $ do
        wrapper <- mount (createElement messageListWithContext { messages: ["Hello World!"] } [])
        E.findReactClass button wrapper >>= E.props

      let
        coerceProps :: Foreign -> { color :: String }
        coerceProps = unsafeCoerce
        color = _.color <<< coerceProps $ btnProps

      assert ("wrong context has been passed: " <> color) (color == "#a0a0a0")

    test "withContext' && getContext" do
      btnProps <- liftEff $ do
        wrapper <- mount (createElement colorProvider { color: "#bbb" } [])
        E.findReactClass button wrapper >>= E.props
      let coerceProps :: Foreign -> { color :: String }
          coerceProps = unsafeCoerce
          color = _.color <<< coerceProps $ btnProps
      assert ("wrong context has been passed: " <> color) (color == "#bbb")

    test "readContext"
      let
        child :: ReactClass Unit
        child = accessContext $ createClass $ (spec unit renderChild) { displayName = "Child" }
        renderChild this = do
          ctx <- readContext (Proxy :: Proxy String) this
          pure $ R.div [ RP.className "child", RP._data { ctx } ] [ R.text ctx ]

        parent :: ReactClass Unit
        parent = withContext "test-string" $ createClass $ (spec unit renderParent) { displayName = "Parent" }
        renderParent this = do
          pure $ createElement child unit []

        readProps :: Foreign -> F { ctx :: String }
        readProps value = do
          ctx <- value ! "data-ctx" >>= readString
          pure { ctx }

      in do
        props <- liftEff $ mount (createElement parent unit []) >>= E.find ".child" >>= E.props
        case runExcept $ readProps props of
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

        readProps :: Foreign -> F { msg :: String }
        readProps value = do
          msg <- value ! "data-msg" >>= readString
          pure { msg }

      in do
        fprops <- liftEff do
          wrp <- mount $ createElement cls { msg: "Hello World!" } []
          E.find ".msg" wrp >>= E.props

        case runExcept $ readProps fprops of
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

      in do
        wrp <- liftEff $ mount $ createElement cls unit []

        ElementId eid <- liftEff do
          element <- E.ref "div" wrp >>= E.getDOMNode 
          liftEff $ Element.id (htmlElementToElement element)

        str <- liftEff $ E.debug wrp
        assert ("wrong element got: " <> eid <> "\n" <> str) $ eid == "ref"

    test "readRef"
      let
        cls :: ReactClass Unit
        cls = createClass $ (spec unit renderFn) { displayName = "Ref" }

        renderFn this = do
          pure $ R.div [ ref "div", RP._id "ref" ] []

      in do
        wrp <- liftEff $ mount (createElement cls unit [])
        inst <- liftEff $ E.instance_ wrp

        let
          refs :: Foreign
          refs = toForeign $ (unsafeCoerce inst).refs

        debugStr <- liftEff $ E.debug wrp

        fnode <- liftEff (unsafeCoerceEff $ readRef "div" refs)
        case runExcept fnode of
          Left _ -> failure ("reference div not found\n" <> debugStr)
          Right node -> do
            ElementId eid <- liftEff $ Element.id (unsafeCoerce node)
            assert ("wrong element got: " <> eid <> "\n" <> debugStr) $ eid == "ref"

  suite "isMounted" 
    let
      render :: forall props e. ReactThis props Boolean -> Eff ( state :: ReactState ReadOnly, props :: ReactProps, refs :: ReactRefs () | e ) ReactElement
      render this = do
        isMnt <- readState this
        pure (R.div' [ R.text (show isMnt) ])

      execute
        :: forall props
         .  Boolean
        -> ReactClass props
        -> props
        -> (ReactWrapper -> Eff (enzyme :: ENZYME, dom :: DOM, avar :: AVAR, console :: CONSOLE, st :: ST stateRef | eff) Unit)
        -> Aff ( console :: CONSOLE, avar :: AVAR, dom :: DOM, enzyme :: ENZYME, st :: ST stateRef | eff) Unit
      execute expected cls props act = do
        st <- liftEff $ do
          wrp <- mount (createElement cls props [])
          act wrp
          st <- E.state wrp
          _ <- E.unmount wrp
          pure st
        case runExcept $ join (readBoolean <$> st) of
          Left err -> failure $ "state reading error: " <> (intercalate " " (renderForeignError <$> err))
          Right isMnt -> equal expected isMnt
    in do
      test "should be false in ComponentWillMount"
        let
          sp :: forall e. ReactSpec Unit Boolean e
          sp =  (spec false render)
            { componentWillMount = \this -> (readIsMounted this >>= writeState this) *> (pure unit) }
        in execute false (createClass $ isMounted sp) unit (\_ -> pure unit)
      test "should be true in componentDidMount"
        let
          sp :: forall e. ReactSpec Unit Boolean e
          sp = (spec false render)
            { componentDidMount = \this -> (readIsMounted this >>= writeState this) *> (pure unit) }
        in execute true (createClass $ isMounted sp) unit (\_ -> pure unit)
      test "should be true in componentWillReceiveProps"
        let
          sp :: forall e. ReactSpec { p :: Int } Boolean e
          sp =  (spec false render)
            { componentWillReceiveProps = \this _ -> (readIsMounted this >>= writeState this) *> (pure unit) }
        in 
          execute true (createClass $ isMounted sp) {p:0} (void <<< E.setProps {p:1})
      test "should be true in componentWillUnmount" do
        ref <- liftEff $ newSTRef false
        let
          sp :: forall e. ReactSpec Unit Boolean (st :: ST stateRef | e)
          sp =  (spec false render)
            { componentWillUnmount = \this -> (readIsMounted this >>= writeSTRef ref) *> (pure unit) }
        isMnt <- liftEff $ do
          _ <- mount (createElement (createClass $ isMounted sp) unit []) >>= E.unmount
          readSTRef ref

        equal true isMnt
