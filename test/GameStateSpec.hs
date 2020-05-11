{-

1) pour tester une opération vous devez construire une "situation initale correcte"
il vous faut générer un e :: EtatDuJeu et un m :: Mob tels que
a) prop_inv_EtatDuJeu e soit True
b) prop_pre_ajouteMob e m  soit True
2) vous calculez le résultat de ajouteMob e m
3) vous testez l'invariant prop_inv_EtatDuJeu (ajouteMob e m) et la postcondition (prop_post_ajouteMob e m)
-}




module StateSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq


import Test.Hspec
import Carte 
import System.Random 
import Foreign.C.Types (CDouble (..) )


import State 

import Control.Exception (evaluate)


carte1 = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nX X    XXX\nXE  X  XXX\nXXXXXXXXXX" ::Carte


gameState1 = initGameState carte1 (mkStdGen 40) (10.0::CDouble)

carteTest1  = do
  describe "Traversalbe" $ do
    it "est " $ do
      
        --
      kart <- carteFromFile "exemple"
      (isTraversable kart 2 8) `shouldBe` True


engineSpec = do
  carteTest1