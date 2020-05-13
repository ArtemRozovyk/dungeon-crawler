
module EnviSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M


import Test.Hspec
import Envi
import Carte

import Control.Exception (evaluate)

envi1 = Envi M.empty

enviTest1  = do
  
  describe "Tests environnement" $ do
    it "Environnement saint" $ do
      prop_envi_inv envi1 `shouldBe` True
    it "pre add_envi" $ do
      pre_ajout_env (C 3 3,(Mob 4 100 100)) envi1  `shouldBe` True
    it "post_add_env " $ do
      let res = ajout_env  (C 3 3,(Mob 4 100 100))  envi1 
      prop_envi_inv res `shouldBe` True
    it "pre_rmv_env " $ do
        let res = ajout_env  (C 3 3,(Mob 4 100 100))  envi1
        pre_rmv_envi (C 3 3) res `shouldBe` True
    it "post_rmv_env " $ do
        let res = ajout_env  (C 3 3,(Mob 4 100 100))  envi1
        let res2 = rmv_coor_envi (C 3 3) res
        post_rmv_envi (C 3 3) res2 `shouldBe` True
        prop_envi_inv res2 `shouldBe` True
    


    


engineSpec = do
  enviTest1