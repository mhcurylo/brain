module BrainSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Brain" $
         it "needs test" $
            "test" `shouldBe` "test"
