
module CarteSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq


import Test.Hspec
import Carte 

import Control.Exception (evaluate)

carte1 = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nX X    XXX\nXE  X  XXX\nXXXXXXXXXX" ::Carte

carteTest1  = do
  

  describe "Tests sur la carte" $ do
    it "Verification d'une case traversable" $ do
      kart <- carteFromFile "exemple"
      (isTraversable kart 2 8) `shouldBe` True
    it "Carte saine" $ do
      kart <- carteFromFile "exemple"
      prop_inv_carte_saine carte1 `shouldBe` True 

    


engineSpec = do
  carteTest1