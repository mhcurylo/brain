{-# LANGUAGE OverloadedStrings #-}
module NameGen (
  runName
) where

import BrainData (Name(..))
import Data.Random.RVar (runRVar, RVar)
import Data.Random.List (randomElement)
import Data.Random.Source.DevRandom (DevRandom(..))
import qualified Data.ByteString                    as B


runName :: IO Name
runName = runRVar getName DevURandom

getName :: RVar Name
getName = do
   title <- randomElement titles
   actor <- randomElement actors
   return $ Name $ B.append title actor

titles :: [B.ByteString]
titles = map (`B.append` " ") ["Brown", "Yellow", "Red", "Green", "Black", "White", "Irish", "Crown", "Savage", "Bonzai"]

actors :: [B.ByteString]
actors = ["Panda", "Aphrodite", "Zeus", "Wolverine", "Cat", "Capybara"]
