

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
        carte <-  (arbitrary :: Gen Carte)
        etat <- (arbitrary :: Gen Etat)
        let envi = envi_tour etat 
        seed <- choose (0, 1000)
        let gen = mkStdGen seed in 
         return  (Model carte envi gen "" K.createKeyboard)


prop_modele_inv_spec :: Property 
prop_modele_inv_spec = forAll (arbitrary::Gen Modele) $ prop_modele_inv


prop_genModele_inv = do
  describe "genTestCarte_QuickCheck" $ do
    it "genere les cartes satisfiant leur invariant" $ 
      property prop_modele_inv_spec


