module ReactHocs.Debug 
  ( debugSpec ) where

import Control.Monad.Eff (Eff)
import Debug.Trace (class DebugWarning, traceA, traceAnyA)
import Prelude (bind, discard, pure, show, (<>))
import React (ReactSpec, ReactThis)
import Unsafe.Coerce (unsafeCoerce)

debugSpec :: forall p s e. DebugWarning => ReactSpec p s e -> ReactSpec p s e
debugSpec spec = spec
  { componentDidMount = componentDidMount
  , componentWillUpdate = componentWillUpdate
  , shouldComponentUpdate = shouldComponentUpdate
  , componentWillUnmount = componentWillUnmount
  }

 where
    displayName :: forall edn. ReactThis p s -> Eff edn String
    displayName this = do
      pure (unsafeCoerce this).__proto__.constructor.displayName

    componentDidMount this = do
      name <- displayName this
      traceA (name <> " componentDidMount")
      spec.componentDidMount this

    componentWillUpdate this props state = do
      name <- displayName this
      traceA (name <> " componentWillUpdate")
      traceAnyA {props, state}
      spec.componentWillUpdate this props state

    shouldComponentUpdate this props state = do
      shouldUpdate <- spec.shouldComponentUpdate this props state
      name <- displayName this
      traceA (name <> " shouldComponentUpdate: " <> show shouldUpdate)
      pure shouldUpdate

    componentWillUnmount this = do
      name <- displayName this
      traceA (name <> " componentWillUnmount")
      spec.componentWillUnmount this
