
module Model where

import SDL
import Data.Set as S
import Keyboard (Keyboard)
import System.Random 
import qualified Keyboard as K
import Carte
import Envi




data Ordre = N | S | E | O | U | R deriving Show


data Modele = Model {carte :: Carte , 
  envi :: Envi , 
  gene :: StdGen , 
  log :: String , 
  keyboard :: Keyboard }


bouge :: Modele -> Entite -> Coord -> Modele
bouge = undefined

decide :: [(Int, Ordre)] -> Modele -> Entite -> Modele
decide = undefined

prevoit_vache :: Modele -> Entite -> [(Int, Ordre)]
prevoit_vache = undefined


{-
data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , virusX :: Int
                           , virusY :: Int
                           , speed :: Int }
  deriving (Show)


initGameState :: IO GameState
initGameState = do
    vX <- randomRIO(40,290)
    vY <- randomRIO(40,290)
    pure $ GameState 200 200 vX vY  3

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ _ _ sp) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ _ _ sp) | px < 320 = gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py _ _ sp) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py _ _ sp) | py < 320 = gs { persoY = py + sp }
                                | otherwise = gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  -- A MODIFIFIER
  let new_gs =if S.member KeycodeZ kbd then moveUp gstate
              else if S.member KeycodeS kbd then moveDown gstate
              else if S.member KeycodeQ kbd then moveLeft gstate
              else if S.member KeycodeD kbd then moveRight gstate
              else gstate
    in
      end_check new_gs

end_check :: GameState -> GameState
end_check gs@(GameState px py vx vy sp) = 
  if(abs(px-vx)<50 && abs(py-vy)<50)
  then GameState px py (-1) (-1) sp
  else gs
-}


