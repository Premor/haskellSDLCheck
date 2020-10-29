{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Image
import Linear ()
import Control.Monad (unless)
import Foreign.C.Types ( CInt )
main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  startPrep renderer
  --appLoop renderer

startPrep renderer = do
    texture <- getRock renderer
    appLoop renderer [initPlayer texture]


data Player = Player  { getPlayerTexture :: Texture
                      , getPlayerX :: CInt
                      , getPlayerY :: CInt
                      , getPlayerHeight :: CInt
                      , getPlayerWeight :: CInt
                      } 


class Movable a where
  move :: a -> CInt -> CInt -> a

instance Movable Player where
  move player x y = Player (getPlayerTexture player)
                           (x + (getPlayerX player))
                           (y + (getPlayerY player))
                           (getPlayerHeight player)
                           (getPlayerWeight player)
                           
                           

class Renderable a where
  getTexture :: a -> Texture
  getRectangle :: a -> Rectangle CInt

instance Renderable Player where
  getTexture player = getPlayerTexture player 
  getRectangle player = Rectangle (P $ V2 (getPlayerX player) (getPlayerY player)) 
                                  (V2 (getPlayerHeight player) (getPlayerWeight player)) 

initPlayer texture = Player texture 10 10 100 100 
-- playerRect::Player -> Rectangle Int
-- playerRect player = Rectangle (P $ V2 (getPlayerX player) (getPlayerY player)) 
--                               (V2 (getPlayerHeight player) (getPlayerWeight player)) 

appLoop ::(Renderable a, Movable a) => Renderer -> [a] -> IO ()
appLoop renderer objects = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
      eventIsWPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeW
          _ -> False
      eventIsAPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeA
          _ -> False
      eventIsSPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeS
          _ -> False
      eventIsDPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeD
          _ -> False
      deltaX = a + d 
        where a = if any eventIsAPress events then -aceleration 
                                   else 0
              d = if any eventIsDPress events then aceleration 
                                   else 0
      deltaY = w + s 
        where w = if any eventIsWPress events then -aceleration 
                                   else 0
              s = if any eventIsSPress events then aceleration 
                                   else 0
      
  rendererDrawColor renderer $= V4 255 255 255 255
  let newPlayer = move player deltaX deltaY
  clear renderer
  --SDL.copy renderer (getTexture player) Nothing (Just $ getRectangle player)
  mapM_ (copyToRender renderer) objects
  present renderer
  unless qPressed (appLoop renderer [newPlayer])
  where
    aceleration = 5
    player = head objects
    copyToRender:: Renderable a => Renderer -> a -> IO ()
    copyToRender renderer object = SDL.copy renderer 
                                            (getTexture object) 
                                            Nothing
                                            (Just $ getRectangle object)  

getRock:: Renderer -> IO Texture
getRock renderer = do 
  rockTexture <- loadTexture renderer "rock.png" -- /home/premor/Desktop/haskell/gameUI/
  return rockTexture