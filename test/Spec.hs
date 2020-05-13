import Test.Hspec
import GameStateSpec as SS
import Carte 
import CarteSpec as CS
import Envi
import EnviSpec as ES
import Model
import ModeleSpec as MS


main :: IO ()
main = hspec $ do
  -- revrev
  CS.engineSpec 
  SS.add_entity_state_test
  ES.engineSpec
  MS.engineSpec