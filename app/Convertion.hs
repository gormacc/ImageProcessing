{-# LANGUAGE RecordWildCards #-}

module Convertion where

import Graphics.UI.WX 
import Graphics.UI.WXCore as W
import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Codec.Picture.Types as J

convertToImage :: J.Image PixelRGB8 -> IO(W.Image ()) 
convertToImage img@Image {..} = imageCreateFromPixels (sz imageWidth imageHeight) (createColorArray img imageWidth imageHeight)

createColorArray :: J.Image PixelRGB8 -> Int -> Int -> [Color]
createColorArray img width height = [ createColor (pixelAt img x y) | y <- [0..height-1], x <- [0..width-1] ]

createColor :: PixelRGB8 -> Color
createColor pixel@(PixelRGB8 r g b) = colorRGB r g b

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