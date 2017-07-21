module ReactHocs.Refs 
  ( ref
  , readRef
  , refFn
  , unsafeSetProp
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.HTML.Types (HTMLElement, readHTMLElement)
import Data.Foreign (F, Foreign)
import Data.Foreign.Index (readProp)
import Prelude (Unit, join, pure, ($), (<$>))
import React (ReactRefs, ReactThis, Read, Write)
import React.DOM.Props (Props, unsafeMkProps)

ref :: String -> Props
ref = unsafeMkProps "ref"

readRef :: forall access eff. String -> Foreign ->  Eff (refs :: ReactRefs (read :: Read | access) | eff) (F HTMLElement) 
readRef name refs = pure $ join $ readHTMLElement <$> readProp name refs

-- | mutate `t` by overwritting a property with a new value
foreign import unsafeSetProp :: forall e a p s. String -> ReactThis p s -> a -> Eff e Unit

-- | example `refCb (unsafeSetProp "childElement" this)`
-- | you can readit back from `this :: ReactThis props state` with `readRef name (toForeign this)`
refFn :: forall access eff. (HTMLElement -> Eff (refs :: ReactRefs (write :: Write | access) | eff) Unit) -> Props
refFn fn = unsafeMkProps "ref" (\node -> unsafePerformEff (fn node))
