module ReactHocs.IsMounted 
  ( readIsMounted
  , writeIsMounted
  , isMounted
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Prelude (Unit, (*>))
import React (ReactSpec, ReactThis)

foreign import writeIsMountedImpl
  :: forall props state e
   . EffFn2 e (ReactThis props state) Boolean Unit

writeIsMounted :: forall props state e. ReactThis props state -> Boolean -> Eff e Unit
writeIsMounted this isMnt = runEffFn2 writeIsMountedImpl this isMnt

foreign import readIsMountedImpl
  :: forall props state e
   . EffFn1 e (ReactThis props state) Boolean

readIsMounted :: forall props state e. ReactThis props state -> Eff e Boolean
readIsMounted this = runEffFn1 readIsMountedImpl this

isMounted
  :: forall props state eff
   . ReactSpec props state eff
  -> ReactSpec props state eff
isMounted spec@{ getInitialState, componentDidMount, componentWillUnmount } =
  spec { getInitialState = \this -> writeIsMounted this false *> getInitialState this
       , componentDidMount = \this -> writeIsMounted this true *> componentDidMount this
       , componentWillUnmount = \this -> componentWillUnmount this *> writeIsMounted this false
       }
