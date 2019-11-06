module Test.Data.Pid where

import Prelude

import Data.Either (Either(..), fromRight)
import Data.Pid (Identifier, Pid, PidConstructionError(..))
import Data.Pid as Pid
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Random.LCG (mkSeed)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

pidTest :: Spec Unit
pidTest =
  describe "Pid" do
    describe "new" do
      it "a valid pid" do
        let pidResult = Pid.new [Tuple 1 2]
        (Pid.identifiers <$> pidResult) `shouldEqual` Right [Tuple 1 2]

      it "out of bounds" do
        let lowPos = Pid.new [Tuple bottom 2]
        lowPos `shouldEqual` Left PidOutOfBounds

        let highPos = Pid.new [Tuple top 2]
        highPos `shouldEqual` Left PidOutOfBounds

        let lowSite = Pid.new [Tuple 1 bottom]
        lowSite `shouldEqual` Left PidOutOfBounds

        let highSite = Pid.new [Tuple 1 top]
        highSite `shouldEqual` Left PidOutOfBounds

    describe "generate" do
      it "between two identifiers" do
        let lower = mkPid [Tuple 1 2]
        let upper = mkPid [Tuple 10 3]
        let site = 4
        let seed = mkSeed 20
        let positionResult = Pid.generate lower upper site seed
        let expected = mkPid [Tuple 6 site]
        positionResult `shouldEqual` expected

      it "when theres no room between identifiers" do
        let lower = mkPid [Tuple 1 2, Tuple 3 3]
        let upper = mkPid [Tuple 2 3, Tuple 4 2]
        let site = 4
        let seed = mkSeed 20
        let positionResult = Pid.generate lower upper site seed
        let expected = mkPid [ Tuple 1 2, Tuple 3 3, Tuple 965420 site ]
        positionResult `shouldEqual` expected

      it "when theres room in the middle" do
          let lower = mkPid [Tuple 1 2, Tuple 3 3, Tuple 1 2]
          let upper = mkPid [Tuple 2 3, Tuple 5 2, Tuple 2 3]
          let site = 4
          let seed = mkSeed 20
          let positionResult = Pid.generate lower upper site seed
          let expected = mkPid [ Tuple 1 2, Tuple 4 4 ]
          positionResult `shouldEqual` expected

mkPid ∷ Array Identifier → Pid
mkPid ids = unsafePartial (fromRight (Pid.new ids))
