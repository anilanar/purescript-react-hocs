module ReactHocs.Class where

class WithContextProps props' props ctx where
  setCtx :: ctx -> props' -> props
