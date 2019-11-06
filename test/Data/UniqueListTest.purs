module Test.Data.UniqueListTest where

import Prelude

import Data.UniqueList as UniqueList
import Random.LCG (mkSeed)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

uniqueListTest ∷ Spec Unit
uniqueListTest =
  let init = UniqueList.empty in
  describe "UniqueList" do
    describe "append" do
      it "can insert into an empty document" do
        let nextList = UniqueList.append init "a" 1 (mkSeed 2)
        UniqueList.toArray nextList `shouldEqual` ["a"]