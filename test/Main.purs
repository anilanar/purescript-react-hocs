module Test.Main where

import Prelude
import Morph
import Test.Unit.Assert as Assert
import Test.Unit (success, suite, test, timeout)
import Test.Unit.Main (runTest)

main = runTest do
  suite "context" do
    test "withContext" do
      success
