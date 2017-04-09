module Recompose.Class where

import Optic.Types (Lens)

class WithContextProps props' props ctx where
  setCtx :: ctx -> props' -> props
