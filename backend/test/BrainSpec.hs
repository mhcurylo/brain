module BrainSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  describe "Brain" $ do
    it "needs test" $ do
      "test" `shouldBe` "test"
