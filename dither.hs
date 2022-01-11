module Main (main) where

import qualified Graphics.Image            as I
import qualified Graphics.Image.Interface  as I.I
import qualified Graphics.Image.ColorSpace as I.CS
import Data.Array
import Data.Ix
import Data.Foldable
import System.IO
import Control.Monad
import System.Console.GetOpt
import System.Exit
import System.Environment
import Data.Maybe

clamp :: Double -> I.Pixel I.Y Double -> I.Pixel I.Y Double
clamp border = I.I.mapPxC (const $ (\x -> if x > border then 1.0 else 0.0) )

dither :: Double -> String -> IO (I.Image I.VU I.CS.Y Double) 
dither border file = do
  -- Read file as a greyscale (Y) image
  toDither <- I.readImageY I.VU file
  
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
    let newPixel = clamp 0.5 oldPixel
        quantErr = I.I.getPxC (oldPixel - newPixel) I.CS.LumaY 
    
    -- Write the clamped pixel
    I.I.write algoImg (x,y) newPixel
    
    -- 
    when (inRange imgBounds (x  ,y+1)) 
         (I.I.read  algoImg (x  ,y+1) >>= \pxl -> 
          I.I.write algoImg (x  ,y+1) $ I.I.liftPx (\x -> x + (7.0/16.0 * quantErr)) pxl) 
    when (inRange imgBounds (x+1,y-1)) 
         (I.I.read  algoImg (x+1,y-1) >>= \pxl -> 
          I.I.write algoImg (x+1,y-1) $ I.I.liftPx (\x -> x + (3.0/16.0 * quantErr)) pxl) 
    when (inRange imgBounds (x+1,y  )) 
         (I.I.read  algoImg (x+1,y  ) >>= \pxl -> 
          I.I.write algoImg (x+1,y  ) $ I.I.liftPx (\x -> x + (5.0/16.0 * quantErr)) pxl) 
    when (inRange imgBounds (x+1,y+1)) 
         (I.I.read  algoImg (x+1,y+1) >>= \pxl -> 
          I.I.write algoImg (x+1,y+1) $ I.I.liftPx (\x -> x + (1.0/16.0 * quantErr)) pxl) 
    return ()

  output <- I.I.freeze algoImg

  return output

data Flag = Input String
          | Output (Maybe String)
          | Cutoff (Maybe Double)

data Options = Options { optInput  :: Maybe String
                       , optOutput :: String
                       , optCutoff :: Double}

defaultOptions :: Options
defaultOptions = Options { optInput  = Nothing 
                         , optOutput = "dither.png"
                         , optCutoff = 0.5 }

options :: [ OptDescr (Options -> Options) ]
options =
  [ Option "i" ["input"]
      (OptArg
        (\arg opt -> opt { optInput = arg })
        "FILE")
      "Input file"

  , Option "o" ["output"]
      (ReqArg
        (\arg opt -> opt { optOutput = arg })
        "FILE")
      "Output file"
  , Option "c" ["cutoff"]
      (ReqArg
        (\arg opt -> opt { optCutoff = read arg})
        "FLOAT")
      "Indicates brightness cutoff"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> putStrLn (concat errs ++ usageInfo header options) >> die "Arguments improperly formatted" 
  where header = "Usage: dither [OPTION..] file"

main :: IO ()
main = do
  argv     <- getArgs
  (o,args) <- compilerOpts argv
  case optInput o of
    Nothing    -> case args of
                    []     -> putStr (usageInfo ("Usage: dither [OPTION..] file") options) >> return ()
                    (x:xs) -> dither (optCutoff o) x >>= I.writeImage (optOutput o)
    Just strIn -> do
      image    <- dither (optCutoff o) (strIn)
      I.writeImage (optOutput o) image
  
