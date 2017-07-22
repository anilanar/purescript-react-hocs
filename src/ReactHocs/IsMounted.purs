module ReactHocs.IsMounted 
  ( isMountedIx
  , isMounted
  , unsafeReadIsMounted
  ) where

import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Data.Symbol (SProxy(..))
import React (ReactSpec, ReactThis)
import React.Ix (ReactSpecIx, ReactThisIx(..), fromReactSpec, getProp, insertPropIx, setPropIx, toReactSpec)
import React.Ix.EffR (EffR)
import Unsafe.Coerce (unsafeCoerce)

unsafeReadIsMounted :: forall props state eff. ReactThis props state -> Eff eff Boolean
unsafeReadIsMounted this = getProp (SProxy :: SProxy "isMounted__") (ReactThisIx this)

isMountedIx
  :: forall props state eff
   . ReactSpec props state eff
  -> ReactSpecIx props state (isMounted__ :: Boolean) (isMounted__ :: Boolean) (isMounted__ :: Boolean) eff
isMountedIx s = f (fromReactSpec s)
  where
    sym :: SProxy "isMounted__"
    sym = SProxy

    unsafeLift :: forall p s e. ReactSpecIx p s () () () e -> ReactSpecIx p s (isMounted__ :: Boolean) (isMounted__ :: Boolean) (isMounted__ :: Boolean) e
    unsafeLift = unsafeCoerce

    unsafeLift' :: forall a e. EffR e {} {isMounted__ :: Boolean} a -> EffR e {isMounted__ :: Boolean} {isMounted__ :: Boolean} a
    unsafeLift' = unsafeCoerce

    f spec
      = let lSpec@{ componentWillMount, componentDidMount, componentWillUnmount } = (unsafeLift spec)
        in lSpec
          { componentWillMount = \this -> insertPropIx sym false this :*> unsafeCoerce (componentWillMount this)
          , componentDidMount = \this -> setPropIx sym true this :*> componentDidMount this
          , componentWillUnmount = \this -> componentWillUnmount this :*> setPropIx sym false this
          }

isMounted
  :: forall props state eff
   . ReactSpec props state eff
  -> ReactSpec props state eff
isMounted s = toReactSpec (isMountedIx s)
