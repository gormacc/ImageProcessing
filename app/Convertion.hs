module Convertion where

import Control.Exception (onException)
import Graphics.UI.WX 
import Graphics.UI.WXCore as W
import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Codec.Picture.Types as J

import CommandHelper
import ImageManipulation
import Paths_ImageProcessing


saveToPng :: W.Image () -> IO ()
saveToPng ioimg = do
	img <- convertToImageRGB8 ioimg
	(savePngImage "/home/maciek/Desktop/img.png" . ImageRGB8 . rotateImg) img 

convertToImageRGB8 :: W.Image () -> IO (J.Image PixelRGB8)
convertToImageRGB8 img = do 
	colors <- imageGetPixels img
	width <- getImageWidth img
	height <- getImageHeight img
	return $ prepareImageRGB8 colors width height

getImageHeight :: W.Image () -> IO Int
getImageHeight img = do
	sizer <- get img size
	return $ sizeH $ sizer

getImageWidth :: W.Image () -> IO Int
getImageWidth img = do
	sizer <- get img size 
	return $ sizeW $ sizer

prepareImageRGB8 :: [Color] -> Int -> Int -> J.Image PixelRGB8
prepareImageRGB8 array width height = generateImage generatorFunction width height where
	generatorFunction :: Int -> Int -> PixelRGB8
	generatorFunction x y = createPixelRGB8  $ array !! (y * width + x)
   
createPixelRGB8 :: Color -> PixelRGB8
createPixelRGB8 color = PixelRGB8 ( fromIntegral $ colorRed color ) ( fromIntegral $ colorGreen color ) ( fromIntegral $ colorBlue color )