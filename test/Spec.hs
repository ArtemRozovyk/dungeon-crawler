import Test.Hspec
import GameStateSpec as SS
import Carte 
import CarteSpec as CS
import Envi
import EnviSpec as ES
import Model
import ModeleSpec as MS
import QuickCheckCarte as QCC
import QuickCheckState as QCS
import QuickCheckModel as QCM


main :: IO ()
main = hspec $ do
  -- revrev
  CS.engineSpec 
  SS.add_entity_state_test
  ES.engineSpec
  MS.engineSpec
  QCC.genCarteSpec
  QCS.genStateSpec
  QCS.addEntitySpec
  QCM.prop_genModele_inv2
  QCM.prop_genModele_inv3
  QCM.prop_genModele_inv4
  