

module QuickCheckModel where

import Test.Hspec
import Test.QuickCheck

import QuickCheckCarte
import QuickCheckState

import State
import Carte
import Envi
import Model

import qualified Data.Map.Strict as M

import System.Random
import Foreign.C.Types (CDouble (..) )
import qualified Keyboard as K


instance Arbitrary Modele where 
    arbitrary = do 
        etat <- (arbitrary :: Gen Etat)
        let envi = envi_tour etat 
        seed <- choose (0, 1000)
        let gen = mkStdGen seed in 
         return  (Model (carte_tour etat) envi gen "" K.createKeyboard)


prop_modele_inv_spec :: Property 
prop_modele_inv_spec = forAll (arbitrary::Gen Modele) $ prop_modele_inv


prop_modele_inv_spec2 :: Property 
prop_modele_inv_spec2 = forAll (arbitrary::Gen Modele) $ prop_modele_inv1

prop_modele_inv_spec3 :: Property 
prop_modele_inv_spec3 = forAll (arbitrary::Gen Modele) $ prop_modele_inv2

prop_modele_inv_spec4 :: Property 
prop_modele_inv_spec4 = forAll (arbitrary::Gen Modele) $ prop_modele_inv4

prop_genModele_inv = do
  describe "genTestCarte_QuickCheck" $ do
    it " invariant" $ 
      property prop_modele_inv_spec


prop_genModele_inv2 = do
  describe "prop_genModele_inv2" $ do
    it  "invariant" $ 
      property prop_modele_inv_spec2


prop_genModele_inv3 = do
  describe "prop_genModele_inv3" $ do
    it " invariant" $ 
      property prop_modele_inv_spec3


prop_genModele_inv4 = do
  describe "prop_genModele_inv4" $ do
    it " invariant" $ 
      property prop_modele_inv_spec4



