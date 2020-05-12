module ModeleSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq


import Test.Hspec
import Carte 
import Envi

import Control.Exception (evaluate)

carte1 = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nX X    XXX\nXE  X  XXX\nXXXXXXXXXX" ::Carte



enviTest1  = do
  

    


engineSpec = do
  enviTest1