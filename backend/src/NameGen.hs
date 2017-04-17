{-# LANGUAGE OverloadedStrings #-}
module NameGen (
  runName
) where

import BrainData
import Data.Random.RVar (runRVar, RVar)
import Data.Random.List (randomElement)
import Data.Random.Source.DevRandom (DevRandom(..))
import qualified Data.Text                      as T


runName :: IO Name
runName = runRVar getName DevURandom

getName :: RVar Name
getName = do
   title <- randomElement titles
   actor <- randomElement actors
   return $ T.append title actor

titles :: [T.Text]
titles = map (`T.append` " ") ["Brown", "Yellow", "Red", "Green", "Black", "White", "Irish", "Crown", "Savage", "Bonzai"]

actors :: [T.Text]
actors = ["panda", "Aphrodite", "Zeus", "wolverine", "cat", "capybara"]
