import Test.Hspec
import GameStateSpec as SS

import Carte 
import CarteSpec as CS


main :: IO ()
main = hspec $ do
  -- revrev
  CS.engineSpec 
  SS.add_entity_state_test