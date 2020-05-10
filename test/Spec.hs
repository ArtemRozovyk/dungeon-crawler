import Test.Hspec

import Carte 
import CarteSpec as CS


main :: IO ()
main = hspec $ do
  -- revrev
  CS.engineSpec 