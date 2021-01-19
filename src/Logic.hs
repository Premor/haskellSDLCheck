{-# LANGUAGE GADTs #-}
module Logic where
import Foreign.C.Types ( CInt )
import SDL 
import Linear ()

data Object a where
  Object:: (Renderable a, Movable a) => a -> Object a


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
                           (x + getPlayerX player)
                           (y + getPlayerY player)
                           (getPlayerHeight player)
                           (getPlayerWeight player)
                           
                           
instance Renderable (Object a) where
  getTexture (Object obj) = getTexture obj
  getRectangle (Object obj) = getRectangle obj 

instance Movable (Object a) where
  move (Object obj) x y = Object $ move obj x y

class Renderable a where
  getTexture :: a -> Texture
  getRectangle :: a -> Rectangle CInt

instance Renderable Player where
  getTexture player = getPlayerTexture player 
  getRectangle player = Rectangle (P $ V2 (getPlayerX player) (getPlayerY player)) 
                                  (V2 (getPlayerHeight player) (getPlayerWeight player)) 

initPlayer :: Texture -> Object Player
initPlayer texture = Object $ Player texture 10 10 100 100 

getX:: V2 a -> a
getX (V2 x _) = x

getY:: V2 a -> a
getY (V2 _ y) = y
