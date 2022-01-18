module Dither where

import qualified Graphics.Image            as I
import qualified Graphics.Image.Interface  as I.I
import qualified Graphics.Image.ColorSpace as I.CS

import Data.Ix
import Data.Foldable

import Control.Monad
import Control.Monad.Primitive


dither :: (RealFrac e, I.I.ColorSpace cs e) => 
  (I.Pixel cs e -> IO (I.Pixel cs e)) -> I.Image I.VU cs e -> IO (I.Image I.VU cs e) 
dither strategy toDither = do
  putStrLn "Beginning dithering"
  -- Get the bounds so we can avoid out of bounds errors
  let (x, y)    = I.I.dims toDither
      imgBounds = ((0,0), (x - 1, y - 1))
  -- Create a mutable copy of the image to work with
  algoImg <- I.I.thaw toDither
  -- Loop over the x and y components of the image
  for_ (range imgBounds) $ \(x, y) -> do
    -- Look at the current pixel
    oldPixel <- I.I.read algoImg (x,y)
    -- Clamp the current pixel and get the error
    newPixel <- strategy oldPixel
    let quantErr = oldPixel - newPixel
    -- Write the clamped pixel
    I.I.write algoImg (x,y) newPixel
    -- Overflow error into downstream pixels 
    when (inRange imgBounds (x  ,y+1)) 
         (I.I.read  algoImg (x  ,y+1) >>= \pxl -> 
          I.I.write algoImg (x  ,y+1) (pxl + (7/16 * quantErr))) 
    when (inRange imgBounds (x+1,y-1)) 
         (I.I.read  algoImg (x+1,y-1) >>= \pxl -> 
          I.I.write algoImg (x+1,y-1) (pxl + (3/16 * quantErr)))
    when (inRange imgBounds (x+1,y  )) 
         (I.I.read  algoImg (x+1,y  ) >>= \pxl -> 
          I.I.write algoImg (x+1,y  ) (pxl + (5/16 * quantErr)))
    when (inRange imgBounds (x+1,y+1)) 
         (I.I.read  algoImg (x+1,y+1) >>= \pxl -> 
          I.I.write algoImg (x+1,y+1) (pxl + (1/16 * quantErr)))
  -- No more modifications, freeze the image and return.
  I.I.freeze algoImg
