module Strategy where

import qualified Graphics.Image            as I
import qualified Graphics.Image.Interface  as I.I 
import qualified Graphics.Image.ColorSpace as I.CS

import System.Random

clamp :: (RealFrac e, I.I.ColorSpace cs e, Monad m) => e -> I.Pixel cs e -> m (I.Pixel cs e)
clamp border = return . I.I.liftPx (\x -> if x > border then 1 else 0)

rainbowClamp :: Double -> I.Pixel I.RGB Double -> IO (I.Pixel I.RGB Double)
rainbowClamp border pxl = do
  if sum pxl < border * 3 then do
    r <- randomRIO (0,0.5)
    g <- randomRIO (0,0.5)
    b <- randomRIO (0,0.5)
    return (I.CS.PixelRGB r g b) 
    else do
    r <- randomRIO (0.5,1) 
    g <- randomRIO (0.5,1)
    b <- randomRIO (0.5,1)
    return (I.CS.PixelRGB r g b)

rainbowStatic :: Double -> I.Pixel I.RGB Double -> IO (I.Pixel I.RGB Double)
rainbowStatic border pxl = do
  rc <- rainbowClamp border pxl
  return (rc/2 + pxl/2)

clampTo :: Double -> I.Pixel I.RGB Double -> I.Pixel I.RGB Double -> I.Pixel I.RGB Double -> IO (I.Pixel I.RGB Double)
clampTo border dark light (I.CS.PixelRGB r g b)
  | r > border && g > border && b > border = return light
  | otherwise                              = return dark

eightCol :: (I.I.ColorSpace cs e, Ord e) => e -> I.Pixel cs e -> IO (I.Pixel cs e)
eightCol border = return . I.I.liftPx (\x -> if x > border then 1 else 0)  
