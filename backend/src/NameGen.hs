{-# LANGUAGE OverloadedStrings #-}
module NameGen (
  runName
) where

import BrainData (Name(..))
import Data.Random.RVar (runRVar, RVar)
import Data.Random.List (randomElement)
import Data.Random.Source.DevRandom (DevRandom(..))
import qualified Data.Text           as T


runName :: IO Name
runName = runRVar getName DevURandom

getName :: RVar Name
getName = do
   title <- randomElement titles
   actor <- randomElement actors
   return $ Name $ T.append title actor

titles :: [T.Text]
titles = map (`T.append` " ") ["Brown", "Yellow", "Red", "Green", "Black", "White", "Irish", "Crown", "Savage", "Bonzai"]

actors :: [T.Text]
actors = ["Panda", "Aphrodite", "Zeus", "Wolverine", "Cat", "Capybara"]
