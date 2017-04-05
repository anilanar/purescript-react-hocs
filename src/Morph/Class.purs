module Morph.Class where

import Optic.Types (Lens')

class WithContextProps props ctx where
  ctxLens :: Lens' props ctx
