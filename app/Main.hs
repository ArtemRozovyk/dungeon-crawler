{-# LANGUAGE OverloadedStrings #-}

module Main where

import Carte
import Control.Monad (unless,foldM,mapM_)
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Mp

import Foreign.C.Types (CInt, CDouble (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model ()
import System.Random
import State


toLoad = ["brick_brown.png","closed_door_eo.png",
    "closed_door_ns.png", "entrance.png","exit.png",
    "open_door_eo.png","open_door_ns.png","player.png",
    "monster.png","chest.png","trap.png"]

wholeScreen = ["background.jpg","lose.png","win.png"]

tiles = ["X","|","-","E","S","/","\\","p","m","t","T"]

mapTiles = Mp.fromList (zip tiles (map (takeWhile (/= '.')) toLoad))


loadGeneric :: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt-> CInt->  IO (TextureMap, SpriteMap)
loadGeneric rdr path tmap smap l h  = do
  let name = takeWhile (/= '.') path 
  let area = (if(name=="background" || name=="win" || name=="lose") then (S.mkArea 0 0 l h) else (S.mkArea 0 0 48 48))
  tmap' <- TM.loadTexture rdr ("assets/used/"++path) (TextureId name) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId name) area
  let smap' = SM.addSprite (SpriteId name) sprite smap
  return (tmap', smap')  




main :: IO ()
main = do
  initializeAll
  carte <-carteFromFile "exemple2"
  let (l,h)=((fromIntegral (cartel carte)*48),(fromIntegral(carteh carte)*48))
  let wSize = V2 l h
  window <- createWindow "Franchir l'Oubliette" $ defaultWindow { windowInitialSize = wSize}
  renderer <- createRenderer window (-1) defaultRenderer 
  (tmap0,smap0) <- (return $ (TM.createTextureMap,SM.createSpriteMap))
  (tmap0',smap0') <- foldM (\(tmp,smp) path -> loadGeneric renderer path tmp smp 0 0 ) (tmap0,smap0) toLoad
  (tmap2,smap2) <- foldM (\(tmp,smp) path -> loadGeneric renderer path tmp smp l h ) (tmap0',smap0') wholeScreen 
  gen  <- getStdGen::IO StdGen 
  moment <- time
  let gameState = initGameState carte gen moment
  gameLoop 60 renderer tmap2 smap2 K.createKeyboard gameState
carte1 = "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nX X    XXX\nXE  X  XXX\nXXXXXXXXXX"
gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard  -> Etat-> IO ()
gameLoop frameRate renderer tmap smap kbd gameState= do
  events <- pollEvents
  currTime <- time
  let (gen,_) =split (gen_tour gameState)
  let kbd' = K.handleEvents events kbd
  let gameState' = etat_tour gameState kbd' currTime gen
  case change_etat gameState of
    Tour nt ct et gt ot lgt -> do
      clear renderer
      S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
      mapM_ (S.displaySprite renderer tmap) (fetchSpritesFromCarte (carte_tour gameState) smap mapTiles)
      mapM_ (S.displaySprite renderer tmap) (fetchSpritesFromEnv gameState smap mapTiles)
      present renderer    
    Perdu -> do
      clear renderer
      S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "lose") smap)
      present renderer
    Gagne -> do
      clear renderer
      S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "win") smap)
      present renderer
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap  K.createKeyboard gameState')

