module Strategy where

import qualified Graphics.Image            as I
import qualified Graphics.Image.Interface  as I.I 
import qualified Graphics.Image.ColorSpace as I.CS

import System.Random
import qualified Graphics.Image.Interface as I.CS

import Control.Monad

import Data.IORef
import Data.Functor ((<&>))
import Data.List (sortBy, maximumBy, minimumBy)
import qualified Data.Map as M

type RGB = I.CS.Components I.RGB Double
type RGBPixel = I.Pixel I.RGB Double

clamp :: (RealFrac e, I.I.ColorSpace cs e, Monad m) => e -> I.Pixel cs e -> m (I.Pixel cs e)
clamp border = return . I.I.liftPx (\x -> if x > border then 1 else 0)

rainbowClamp :: Double -> RGBPixel -> IO RGBPixel
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

rainbowStatic :: Double -> RGBPixel -> IO RGBPixel
rainbowStatic border pxl = do
  rc <- rainbowClamp border pxl
  return (rc/2 + pxl/2)

clampTo :: Double -> RGBPixel -> RGBPixel -> RGBPixel -> IO RGBPixel
clampTo border dark light (I.CS.PixelRGB r g b)
  | r > border && g > border && b > border = return light
  | otherwise                              = return dark

eightCol :: (I.I.ColorSpace cs e, Ord e) => e -> I.Pixel cs e -> IO (I.Pixel cs e)
eightCol border = return . I.I.liftPx (\x -> if x > border then 1 else 0)  

rgbEuclidDistance :: RGB -> RGB -> Double
rgbEuclidDistance (r1, g1, b1) (r2, g2, b2) = sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

getNewCentroid :: [RGB] -> [RGB] -> RGB
getNewCentroid centroids = maximumBy (\c1 c2 -> compare (minimum $ map (rgbEuclidDistance c1) centroids) (minimum $ map (rgbEuclidDistance c2) centroids))

initializeKMeans :: [RGB] -> Int -> [RGB] 
initializeKMeans dats k
  | k > 0     = initRec [head dats] dats (k - 1)
  | otherwise = []
  where
    initRec :: [RGB] -> [RGB] -> Int -> [RGB]
    initRec centroids _ 0 = centroids
    initRec centroids dats k = initRec (getNewCentroid centroids dats:centroids) dats (k - 1)

getClosest :: RGB -> [RGB] -> RGB
getClosest color = minimumBy (\c1 c2 -> compare (rgbEuclidDistance c1 color) (rgbEuclidDistance c2 color))

cluster :: [RGB] -> [RGB] -> M.Map RGB [RGB] 
cluster centroids dats = M.fromListWith (++) $ map ( \x -> (getClosest x centroids, [x]) ) dats

strictAdd :: RGB -> RGB -> RGB
strictAdd (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

constMult :: Double -> RGB -> RGB
constMult c (r1, g1, b1) = (c * r1, c * g1, c * b1)

getMean :: [RGB] -> RGB
getMean colors = constMult ( 1 / fromIntegral (length colors) ) ( foldl1 strictAdd colors )
 
kMeans :: Int -> [RGB] -> [RGB]
kMeans k dats = let
  initialCentroids = initializeKMeans dats k
  in kMeansRec 10 (cluster initialCentroids dats)
 where
  kMeansRec :: Int -> M.Map RGB [RGB] -> [RGB]
  kMeansRec 0 map    = M.keys map
  kMeansRec k oldMap = let
   newCentroids = map getMean $ M.elems oldMap
     in 
     if newCentroids == M.keys oldMap 
     then M.keys oldMap
     else let
          newMap = cluster newCentroids (concat $ M.elems oldMap)
          in kMeansRec (k - 1) newMap

approximateBy :: [RGB] -> RGBPixel -> RGBPixel
approximateBy palette pixel = 
  let
  color = I.I.toComponents pixel
  in I.I.fromComponents (getClosest color palette)
