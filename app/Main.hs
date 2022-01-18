module Main (main) where

import Dither
import Strategy

import qualified Graphics.Image            as I
import qualified Graphics.Image.Interface  as I.I 
import qualified Graphics.Image.ColorSpace as I.CS

import System.IO
import System.Console.GetOpt
import System.Exit
import System.Environment
import System.Random
import System.FilePath

import Control.Monad

import Data.Foldable
import Data.Maybe

data Options = Options { optInput   :: Maybe String
                       , optOutput  :: String
                       , optFile    :: String
                       , optKMeans  :: Maybe Int
                       , optCutoff  :: Double
                       , optRainbow :: Bool
                       , optTwoCol  :: Maybe (I.Pixel I.RGB Double, I.Pixel I.RGB Double)
                       , optHelp    :: Bool 
                       , optEight   :: Bool }
                       

defaultOptions :: Options
defaultOptions = Options { optInput   = Nothing 
                         , optOutput  = "dither"
                         , optFile    = "png"
                         , optKMeans  = Nothing
                         , optCutoff  = 0.5 
                         , optRainbow = False
                         , optTwoCol  = Nothing
                         , optHelp    = False 
                         , optEight   = False}

readTwoCol :: String -> (I.Pixel I.RGB Double, I.Pixel I.RGB Double)
readTwoCol xs = 
  let vals = map read . words $ xs in
      (I.CS.PixelRGB (head vals) (vals !! 1) (vals !! 2) 
      ,I.CS.PixelRGB (vals !! 3) (vals !! 4) (vals !! 5))

options :: [ OptDescr (Options -> Options) ]
options =
  [ Option "h" ["help"]     (NoArg  (\opt -> opt { optHelp = True } ))                                        "View this help dialogue"
  , Option "i" ["input"]    (ReqArg (\arg opt -> opt { optInput = Just arg })               "FILE")           "Input file"
  , Option "o" ["output"]   (ReqArg (\arg opt -> opt { optOutput = arg })                   "FILE")           "Output prefix"
  , Option "f" ["filetype"] (ReqArg (\arg opt -> opt { optFile = arg })                     "FILETYPE")       "Output filetype" 
  , Option "k" ["kmeans"]   (ReqArg (\arg opt -> opt { optKMeans = Just $ read arg })       "INT")   "Gives a K-means based dithered approximation"
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
      ditherFunc 
        | isJust (optKMeans o) = case optKMeans o of
                                   Just k  -> (\img ->
                                     let palette = kMeans k (map I.I.toComponents . concat . I.toLists $ img)
                                      in dither (return . approximateBy palette) img )
                                   Nothing -> error "K means has argument, but option -k was not set"
        | optRainbow o = dither (rainbowStatic cutoff) 
        | optEight o   = dither (eightCol cutoff)
        | otherwise    = case optTwoCol o of
                          Just (dark, light) -> dither (clampTo cutoff dark light)
                          Nothing            -> dither (clampTo cutoff (I.CS.PixelRGB 0 0 0) (I.CS.PixelRGB 1 1 1))
      targets    = case optInput o of
                     Nothing -> args
                     Just strIn -> strIn:args
  for_ targets $ \tgt -> do
    I.readImage' tgt >>= ditherFunc >>= I.writeImage (prefix ++ takeBaseName tgt ++ "." ++ optFile o)
