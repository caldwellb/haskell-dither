module Main (main) where

import qualified Graphics.Image            as I
import qualified Graphics.Image.Interface  as I.I
import qualified Graphics.Image.ColorSpace as I.CS

import Data.List
import Data.Array
import Data.Ix
import Data.Foldable
import Data.Maybe

import System.IO
import System.Console.GetOpt
import System.Exit
import System.Environment
import System.Random

import Control.Monad
import Control.Monad.Primitive

clamp :: (RealFrac e, I.I.ColorSpace cs e, Monad m) => e -> I.Pixel cs e -> m (I.Pixel cs e)
clamp border pxl = return $ I.I.mapPxC (const $ (\x -> if x > border then 1 else 0) ) pxl

rainbowClamp :: Double -> I.Pixel I.RGB Double -> IO (I.Pixel I.RGB Double)
rainbowClamp border pxl = do
  if I.CS.toPixelY pxl < I.CS.PixelY border then do
    r <- randomRIO (0,0.5)
    g <- randomRIO (0,0.5)
    b <- randomRIO (0,0.5)
    return (I.CS.PixelRGB r g b) 
    else do
    r <- randomRIO (0.5,1) 
    g <- randomRIO (0.5,1)
    b <- randomRIO (0.5,1)
    return (I.CS.PixelRGB r g b)

clampTo :: Double -> I.Pixel I.RGB Double -> I.Pixel I.RGB Double -> I.Pixel I.RGB Double -> IO (I.Pixel I.RGB Double)
clampTo border dark light pxl = if I.CS.toPixelY pxl < I.CS.PixelY border then return dark else return light

dither :: (RealFrac e, I.I.ColorSpace cs e, PrimMonad m) => 
  e -> (I.Pixel cs e -> m (I.Pixel cs e)) -> I.Image I.VU cs e -> m (I.Image I.VU cs e) 
dither bound strategy toDither = do
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
    algPixel <- clamp bound oldPixel
    let quantErr = oldPixel - algPixel
    -- Write the clamped pixel
    I.I.write algoImg (x,y) newPixel
    -- Overflow error into downstream pixels 
    when (inRange imgBounds (x  ,y+1)) 
         (I.I.read  algoImg (x  ,y+1) >>= \pxl -> 
          I.I.write algoImg (x  ,y+1) $ (pxl + (7/16 * quantErr))) 
    when (inRange imgBounds (x+1,y-1)) 
         (I.I.read  algoImg (x+1,y-1) >>= \pxl -> 
          I.I.write algoImg (x+1,y-1) $ (pxl + (3/16 * quantErr)))
    when (inRange imgBounds (x+1,y  )) 
         (I.I.read  algoImg (x+1,y  ) >>= \pxl -> 
          I.I.write algoImg (x+1,y  ) $ (pxl + (5/16 * quantErr)))
    when (inRange imgBounds (x+1,y+1)) 
         (I.I.read  algoImg (x+1,y+1) >>= \pxl -> 
          I.I.write algoImg (x+1,y+1) $ (pxl + (1/16 * quantErr)))
    return ()
  -- No more modifications, freeze the image and return.
  I.I.freeze algoImg

data Flag = Input String
          | Output (Maybe String)
          | Cutoff (Maybe Double)

data Options = Options { optInput   :: Maybe String
                       , optOutput  :: String
                       , optCutoff  :: Double
                       , optRainbow :: Bool
                       , optTwoCol  :: Maybe (I.Pixel I.RGB Double, I.Pixel I.RGB Double)}

defaultOptions :: Options
defaultOptions = Options { optInput   = Nothing 
                         , optOutput  = "dither"
                         , optCutoff  = 0.5 
                         , optRainbow = False
                         , optTwoCol  = Nothing }

readTwoCol :: String -> (I.Pixel I.RGB Double, I.Pixel I.RGB Double)
readTwoCol xs = 
  let vals = map read . words $ xs in
      (I.CS.PixelRGB (vals !! 0) (vals !! 1) (vals !! 2) 
      ,I.CS.PixelRGB (vals !! 3) (vals !! 4) (vals !! 5))

options :: [ OptDescr (Options -> Options) ]
options =
  [ Option "i" ["input"]
      (OptArg (\arg opt -> opt { optInput = arg }) "FILE")
      "Input file"

  , Option "o" ["output"]
      (ReqArg
        (\arg opt -> opt { optOutput = arg })
        "FILE")
      "Output prefix"

  , Option "c" ["cutoff"]
      (ReqArg
        (\arg opt -> opt { optCutoff = read arg})
        "FLOAT")
      "Indicates brightness cutoff"
  
  , Option "r" ["rainbow"]
      (NoArg (\opts -> opts {optRainbow = True}))
      "use random colors in place of white"
  
  , Option "t" ["twocolor"]
      (OptArg 
        (\arg opt -> opt { optTwoCol = readTwoCol <$> arg } ) "\"DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE\"")
      "RGB for dark and light sections"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> putStrLn (concat errs ++ usageInfo header options) >> die "" 
  where header = "Usage: dither [OPTION..] file"

main :: IO ()
main = do
  argv     <- getArgs
  print argv
  (o,args) <- compilerOpts argv
  mapM_ putStrLn args
  let cutoff     = optCutoff o
      prefix     = optOutput o
      ditherFunc = if (optRainbow o) 
                    then dither cutoff (rainbowClamp cutoff) 
                    else case (optTwoCol o) of
                           Just (dark, light) -> dither cutoff (clampTo cutoff dark light)
                           Nothing            -> dither cutoff (clamp cutoff)
      targets    = case optInput o of
                     Nothing -> args
                     Just strIn -> strIn:args
  for_ targets $ \tgt -> do
    I.readImage' tgt >>= ditherFunc >>= I.writeImage (prefix ++ (takeWhile (/= '.') tgt) ++ ".png")

  
