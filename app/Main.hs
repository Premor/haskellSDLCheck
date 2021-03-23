{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Image
import Linear ()
import Control.Monad (unless,when)
import Foreign.C.Types ( CInt )
import Logic
import Data.Maybe ( fromJust )

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow { windowInitialSize = V2 800 600 }
  renderer <- createRenderer window (-1) defaultRenderer
  startPrep renderer window
  --appLoop renderer

startPrep renderer window = do
    texture <- getRock renderer
    appLoop renderer window [Logic.initPlayer texture]




-- data Application = Application  { applicationWindow :: Window 
--                                 , applicationRenderer :: Renderer
--                                 ,
-- }




-- playerRect::Player -> Rectangle Int
-- playerRect player = Rectangle (P $ V2 (getPlayerX player) (getPlayerY player)) 
--                               (V2 (getPlayerHeight player) (getPlayerWeight player)) 

appLoop ::Renderer -> Window -> [Object a] -> IO ()
appLoop renderer window objects = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
         
  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  renderBackground renderer 
  -- mapM_ (copyToRender renderer) objects
  renderGUI renderer window
  present renderer
  unless qPressed (appLoop renderer window objects)
  where
    aceleration = 5
    player = head objects
    copyToRender:: Renderer -> Object a -> IO ()
    copyToRender renderer object = SDL.copy renderer 
                                            (getTexture object) 
                                            Nothing
                                            (Just $ getRectangle object)  

getRock:: Renderer -> IO Texture
getRock renderer = do loadTexture renderer "rock.png" -- /home/premor/Desktop/haskell/gameUI/


renderBackground :: Renderer -> IO ()
renderBackground renderer = do
    bgTexture <- loadTexture renderer "Background.jpg" 
    SDL.copy renderer bgTexture Nothing Nothing

renderGUI :: Renderer -> Window -> IO ()
renderGUI renderer window = do
  size <- get $ windowSize window 
  renderExitButton renderer size
  return () 
    

renderExitButton:: Renderer -> V2 CInt -> IO ()
renderExitButton renderer rendererSize = do
  buttonTexture <- loadTexture renderer "rock.png"
  SDL.copy renderer buttonTexture Nothing $ Just $ Rectangle (P $ V2 x y) (V2 w h)
  return ()
  where
    h = 40
    w = 100
    x = getX rendererSize - w - 10
    y = 0 -- getY rendererSize - h