module ReactHocs.IsMounted 
  ( isMountedIx
  , isMounted
  , unsafeReadIsMounted
  ) where

import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Data.Symbol (SProxy(..))
import Prelude (($), (<<<))
import React (ReactSpec, ReactThis)
import React.Ix (ReactSpecIx, ReactThisIx(ReactThisIx), fromReactSpec, getProp, insertPropIx, setPropIx)
import React.Ix.EffR (EffR)
import React.Ix.Subrow (class Subrow)
import React.Ix.UnionReactIx ((:<>))
import Type.Row (class RowLacks, RProxy)
import Unsafe.Coerce (unsafeCoerce)

unsafeReadIsMounted :: forall props state eff. ReactThis props state -> Eff eff Boolean
unsafeReadIsMounted this = getProp (SProxy :: SProxy "isMounted__") (ReactThisIx this)

-- I need a different function, one in which one already is using isMounted and one just wants to add logic for it so it works.
-- but for that one would need `getInitialState` to be defined before we know the state.
isMountedIx
  :: forall props state i r o eff
   . RowLacks ("isMounted__") i
  => RowLacks ("isMounted__") r
  => RowLacks ("isMounted__") o
  => Subrow i r
  => Subrow o r
  => ReactSpecIx props state i r o eff
  -> ReactSpecIx props state (isMounted__ :: Boolean | i) (isMounted__ :: Boolean | r) (isMounted__ :: Boolean | o) eff
isMountedIx s =
    { render: \this -> coerceRender (s.render (unsafeCoerce this))
    , displayName: s.displayName
    , getInitialState: coerceI <<< s.getInitialState <<< unsafeCoerce -- limit
    , componentWillMount: \this -> insertPropIx sym false this :<> (s.componentWillMount this)
    , componentDidMount: \this -> (setPropIx sym true this) :*> (coerceR $ s.componentDidMount (unsafeCoerce this))
    , componentWillReceiveProps: \this props -> coerceR $ s.componentWillReceiveProps (unsafeCoerce this) props
    , shouldComponentUpdate: \this props state -> coerceR $ s.shouldComponentUpdate (unsafeCoerce this) props state
    , componentWillUpdate: \this props state -> coerceR $ s.componentWillUpdate (unsafeCoerce this) props state
    , componentDidUpdate: \this props state -> coerceR $ s.componentDidUpdate (unsafeCoerce this) props state
    , componentWillUnmount: \this -> coerceO (s.componentWillUnmount (unsafeCoerce this)) :*> (coerceRO $ setPropIx sym false this)
    }
  where
    coerceRender :: forall e a. EffR e (RProxy i) (RProxy r) a -> EffR e (RProxy (isMounted__ :: Boolean | i)) (RProxy (isMounted__ :: Boolean | r)) a
    coerceRender = unsafeCoerce

    coerceI :: forall e a. EffR e (RProxy i) (RProxy i) a -> EffR e (RProxy (isMounted__ :: Boolean | i)) (RProxy (isMounted__ :: Boolean | i)) a
    coerceI = unsafeCoerce

    coerceR :: forall e a. EffR e (RProxy r) (RProxy r) a -> EffR e (RProxy (isMounted__ :: Boolean | r)) (RProxy (isMounted__ :: Boolean | r)) a
    coerceR = unsafeCoerce

    coerceO :: forall e. EffR e (RProxy r) (RProxy o) (ReactThisIx props state o) -> EffR e (RProxy (isMounted__ :: Boolean | r)) (RProxy (isMounted__ :: Boolean | o)) (ReactThisIx props state (isMounted__ :: Boolean | o))
    coerceO = unsafeCoerce

    coerceRO :: forall e. EffR e (RProxy (isMounted__ :: Boolean | r)) (RProxy (isMounted__ :: Boolean | r)) (ReactThisIx props state (isMounted__ :: Boolean | r)) -> EffR e (RProxy (isMounted__ :: Boolean | o)) (RProxy (isMounted__ :: Boolean | o)) (ReactThisIx props state (isMounted__ :: Boolean | o))
    coerceRO = unsafeCoerce


    sym :: SProxy "isMounted__"
    sym = SProxy

isMounted
  :: forall props state eff
   . ReactSpec props state eff
  -> ReactSpecIx props state (isMounted__ :: Boolean) (isMounted__ :: Boolean) (isMounted__ :: Boolean) eff
isMounted s = (isMountedIx (fromReactSpec s))
