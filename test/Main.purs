module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Data.ArrayExtensionsTest (arrayExtensionsTest)
import Test.Data.UniqueListTest (uniqueListTest)
import Test.Data.Pid (pidTest)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  pidTest
  arrayExtensionsTest
  uniqueListTest
