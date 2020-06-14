

module QuickCheckState where

import Test.Hspec
import Test.QuickCheck

import State
import Carte
import Envi
import QuickCheckCarte 

import qualified Data.Map.Strict as M

import System.Random
import Foreign.C.Types (CDouble (..) )



instance Arbitrary Etat where 
    arbitrary = do 
         carte <-  (arbitrary :: Gen Carte)
         moment <- choose (0::CDouble,20::CDouble)
         numberOfMobs <- choose(2,6) 
         seed <- choose (0, 1000)
         let gen = mkStdGen seed 
         return $ init_state carte numberOfMobs moment gen


instance Arbitrary Entite where 
  arbitrary = do 
    id <-choose(1,41) 
    return (Mob id 100 1)

prop_genState_inv :: Etat -> Property 
prop_genState_inv e = 
  let mobNumber= M.size $ contenu_envi $  envi_tour e in 
        classify (mobNumber ==5)  "For 2 mobs" $ -- 3 pour le joueur, trap et trésor
        classify (mobNumber ==6)  "For 3 mobs" $ 
        classify (mobNumber ==7)  "For 4 mobs" $ 
        classify (mobNumber ==8)  "For 5 mobs" $ 
        classify (mobNumber ==9)  "For 6 mobs" $ 

    property $ prop_state_inv e 


genStateSpec = do
  describe "State generator QuickCheck invariant" $ do
    it "Teste l'invariant pour les états générées aléatoirement" $ 
      property prop_genState_inv


prop_add_entity  :: Etat ->Entite-> Coord -> Property 
prop_add_entity e ent c = 
  (prop_pre_add_entity_state e c ent)
  ==> 
  let mobNumber= M.size $ contenu_envi $  envi_tour e in 
        classify (mobNumber ==5)  "For 2 mobs" $ -- 3 pour le joueur, trap et trésor
        classify (mobNumber ==6)  "For 3 mobs" $ 
        classify (mobNumber ==7)  "For 4 mobs" $ 
        classify (mobNumber ==8)  "For 5 mobs" $ 
        classify (mobNumber ==9)  "For 6 mobs" $ 
    let env_res = add_entity_state e c ent in 
    property $ prop_post_add_entity_state env_res c ent 

addEntitySpec = do
  describe "Add entity post et pre" $ do
    it "L'ajout arbitraire d'entite est verifié" $
      property prop_add_entity 

