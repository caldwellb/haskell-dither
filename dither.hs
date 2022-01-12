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
import System.FilePath

import Control.Monad
import Control.Monad.Primitive

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

dither :: (RealFrac e, I.I.ColorSpace cs e, PrimMonad m) => 
  (I.Pixel cs e -> m (I.Pixel cs e)) -> I.Image I.VU cs e -> m (I.Image I.VU cs e) 
dither strategy toDither = do
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
                       , optFile    :: String
                       , optCutoff  :: Double
                       , optRainbow :: Bool
                       , optTwoCol  :: Maybe (I.Pixel I.RGB Double, I.Pixel I.RGB Double)
                       , optHelp    :: Bool 
                       , optEight   :: Bool }
                       

defaultOptions :: Options
defaultOptions = Options { optInput   = Nothing 
                         , optOutput  = "dither"
                         , optFile    = "png"
                         , optCutoff  = 0.5 
                         , optRainbow = False
                         , optTwoCol  = Nothing
                         , optHelp    = False 
                         , optEight   = False}

readTwoCol :: String -> (I.Pixel I.RGB Double, I.Pixel I.RGB Double)
readTwoCol xs = 
  let vals = map read . words $ xs in
      (I.CS.PixelRGB (vals !! 0) (vals !! 1) (vals !! 2) 
      ,I.CS.PixelRGB (vals !! 3) (vals !! 4) (vals !! 5))

options :: [ OptDescr (Options -> Options) ]
options =
  [ Option "h" ["help"]     (NoArg  (\opt -> opt { optHelp = True } ))                                        "View this help dialogue"
  , Option "i" ["input"]    (ReqArg (\arg opt -> opt { optInput = Just arg })               "FILE")           "Input file"
  , Option "o" ["output"]   (ReqArg (\arg opt -> opt { optOutput = arg })                   "FILE")           "Output prefix"
  , Option "f" ["filetype"] (ReqArg (\arg opt -> opt { optFile = arg })                     "FILETYPE")       "Output filetype" 
  , Option "c" ["cutoff"]   (ReqArg (\arg opt -> opt { optCutoff = read arg})               "FLOAT")          "Indicates brightness cutoff"
  , Option "r" ["rainbow"]  (NoArg  (\opts -> opts {optRainbow = True}))                                      "use random colors in place of white, has highest priority"  
  , Option "e" ["eight"]    (NoArg  (\opts -> opts {optEight = True}))                                        "Use one bit for R,G,B colors, giving 8 total colors"
  , Option "w" ["twocolor"] (ReqArg (\arg opt -> opt { optTwoCol = Just $ readTwoCol arg } ) "\"6 DOUBLES\"") "RGB for dark and light sections"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> hPutStr stderr (concat errs ++ usageInfo header options) >> exitFailure 
  where header = "Usage: dither [OPTION..] file"

main :: IO ()
main = do
  argv     <- getArgs
  (o,args) <- compilerOpts argv
  when (optHelp o) (do
    hPutStr stderr (usageInfo "Usage: dither [OPTION..] file" options)
    exitSuccess)
  let cutoff     = optCutoff o
      prefix     = optOutput o
      ditherFunc = if (optRainbow o) 
                    then dither (rainbowStatic cutoff) 
                    else if (optEight o) 
                          then dither (eightCol cutoff)
                          else case (optTwoCol o) of
                                Just (dark, light) -> dither (clampTo cutoff dark light)
                                Nothing            -> dither (clampTo cutoff (I.CS.PixelRGB 0 0 0) (I.CS.PixelRGB 1 1 1))
      targets    = case optInput o of
                     Nothing -> args
                     Just strIn -> strIn:args
  for_ targets $ \tgt -> do
    I.readImage' tgt >>= ditherFunc >>= I.writeImage (prefix ++ (takeBaseName tgt) ++ "." ++ optFile o)
