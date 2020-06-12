module QuickCheckCarte where

import Test.Hspec
import Test.QuickCheck

import Carte
import System.Random






prop_revapp :: Eq a => [a] -> [a] -> Bool
prop_revapp xs ys = reverse (xs <> ys) == reverse ys <> reverse xs

revrevSpec = do
  describe "reverse" $ do
    context "when used with ints" $ do
      it "is idempotent" $ property $          
        \xs ys ->
        prop_revapp (xs :: [Int]) ys 

genCarte :: Gen Carte 
genCarte = do 
    seed <- choose (0, 1000)
    let gen = mkStdGen seed 
    return (generateCarteVerified gen)
    
instance Arbitrary Carte where
    arbitrary = genCarte

instance Arbitrary Coord where
    arbitrary =  do
        x <- choose (1,10)
        y <- choose (0,10)
        return (C x y)

prop_genCarte_inv :: Property 
prop_genCarte_inv = forAll genCarte $ prop_inv_carte_saine


genCarteSpec = do
  describe "Carte générateur QuickCheck invariant" $ do
    it "Teste l'invariant pour les cartés générées aléatoirement" $ 
      property prop_genCarte_inv






