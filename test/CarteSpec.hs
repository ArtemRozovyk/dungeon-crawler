
module CarteSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq


import Test.Hspec
import Carte 

import Control.Exception (evaluate)

carte1 = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nX X    XXX\nXE  X  XXX\nXXXXXXXXXX" ::Carte




carteTest2  = do
  describe "Tests sur la carte" $ do
    it "Cases comprises dans la carte" $ do
      prop_carte1 carte1 `shouldBe` True 
    it "Chaque valeur dans les limites a une valeur" $ do
      prop_carte2 carte1 `shouldBe` True 
    it "Unique entree/sortie" $ do
      prop_carte3 carte1 `shouldBe` True 
    it "Entouré des murs" $ do
      prop_carte4 carte1 `shouldBe` True 
    it "Chaque porte a deux murs autours" $ do
      prop_carte5 carte1 `shouldBe` True 
    it "Sortie acc. depuis entrée" $ do
      prop_carte6 carte1 `shouldBe` True 


carteTest1  = do
  

  describe "Tests sur la carte" $ do
    it "Verification d'une case traversable" $ do
      (isTraversable carte1 2 8) `shouldBe` True
    it "Carte saine" $ do
      prop_inv_carte_saine carte1 `shouldBe` True 

    


engineSpec = do
  carteTest2
  --carteTest2