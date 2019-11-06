module Test.Data.ArrayExtensionsTest where

import Prelude

import Data.ArrayExtensions (splitWhere)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

arrayExtensionsTest :: Spec Unit
arrayExtensionsTest =
  describe "ArrayExtensions" do
    describe "splitWhere" do
      it "does not match anything" do
        let {left, curr, right} = splitWhere [1] (const false)
        left `shouldEqual` [1]
        curr `shouldEqual` Nothing
        right `shouldEqual` []

      it "matches in the middle" do
        let {left, curr, right} = splitWhere [1, 2, 3, 4] (_ == 3)
        left `shouldEqual` [1, 2]
        curr `shouldEqual` (Just 3)
        right `shouldEqual` [4]