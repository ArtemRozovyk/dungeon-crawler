
module CarteSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq


import Test.Hspec
import Carte 

import Control.Exception (evaluate)


carteTest1  = do
  describe "Traversalbe" $ do
    it "est " $ do
      kart <- carteFromFile "exemple"
      (isTraversable kart 2 8) `shouldBe` True
    it "est " $ do
      kart <- carteFromFile "exemple"
      (isTraversable kart 2 8) `shouldBe` True


engineSpec = do
  carteTest1