{-# LANGUAGE OverloadedStrings #-}

module Main where
import Carte
import Control.Monad (unless,foldM,mapM_)
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Mp

import Foreign.C.Types (CInt (..) )

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
import qualified Model as M

import State


toLoad = ["brick_brown.png","closed_door_eo.png",
    "closed_door_ns.png", "entrance.png","exit.png",
    "open_door_eo.png","open_door_ns.png","player.png","monster.png"]; 
tiles = ["X","|","-","E","S","/","\\","p","m"]
mapTiles = Mp.fromList (zip tiles (map (takeWhile (/= '.')) toLoad))

loadGeneric :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadGeneric rdr path tmap smap  = do
  let name = takeWhile (/= '.') path 
  let area = (if(name=="background") then (S.mkArea 0 0 480 480) else (S.mkArea 0 0 48 48))
  tmap' <- TM.loadTexture rdr ("assets/used/"++path) (TextureId name) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId name) area
  let smap' = SM.addSprite (SpriteId name) sprite smap
  return (tmap', smap')  

main :: IO ()
main = do
  initializeAll
  carte <-carteFromFile "exemple"
  window <- createWindow "Franchir l'Oubliette" $ defaultWindow { windowInitialSize = V2 480 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  (tmap0,smap0) <- (return $ (TM.createTextureMap,SM.createSpriteMap))
  (tmap0',smap0') <- foldM (\(tmp,smp) path -> loadGeneric renderer path tmp smp ) (tmap0,smap0) toLoad
  (tmap, smap) <- loadGeneric renderer "background.jpg" tmap0' smap0'

  --putStrLn (show (carteh carte))
  -- chargement de l'image du fond
  -- initialisation de l'état du jeu
  --gameState <- M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  --putStrLn $ concat $ testCarte carte
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd carte

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard  -> Carte -> IO ()
gameLoop frameRate renderer tmap smap kbd carte= do
  gameState <- initGameState carte
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --- display carte 
  mapM_ (S.displaySprite renderer tmap) (fetchSpritesFromCarte carte smap mapTiles)
  mapM_ (S.displaySprite renderer tmap) (fetchSpritesFromEnv gameState smap mapTiles)

  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  --let gameState' = M.gameStep gameState kbd' deltaTime
  
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' carte)
