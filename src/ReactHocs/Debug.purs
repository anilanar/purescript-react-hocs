module ReactHocs.Debug 
  ( debugSpec ) where

import Control.Monad.Eff (Eff)
import Debug.Trace (traceA, traceAnyA)
import Prelude (bind, discard, pure, show, (<>))
import React (ReactSpec, ReactThis)
import Unsafe.Coerce (unsafeCoerce)

debugSpec :: forall p s e. ReactSpec p s e -> ReactSpec p s e
debugSpec spec = spec
  { componentDidMount = componentDidMount
  , componentWillUpdate = componentWillUpdate
  , shouldComponentUpdate = shouldComponentUpdate
  }

 where
    displayName :: forall edn. ReactThis p s -> Eff edn String
    displayName this = do
      pure (unsafeCoerce this).__proto__.constructor.name

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
