{-# LANGUAGE RecordWildCards #-}

module Convertion where

import Graphics.UI.WX 
import Graphics.UI.WXCore as W
import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Codec.Picture.Types as J
import System.FilePath.Posix

-- | Convert JuicyPixels image to WXCore image 
convertToImage :: J.Image PixelRGB8 -- ^ Given JuicyPixel image
			   -> IO(W.Image ()) -- ^ Created WXCore image 
convertToImage img@Image {..} = imageCreateFromPixels (sz imageWidth imageHeight) (createColorArray img imageWidth imageHeight)

-- | Retreive all pixel colors from JuicyPixel imaeg
createColorArray :: J.Image PixelRGB8 -- ^ Given JuicyPixel image 
				 -> Int -- ^ Width of given image 
				 -> Int -- ^ Height of given image
				 -> [Color] -- ^ Color array
createColorArray img width height = [ createColor (pixelAt img x y) | y <- [0..height-1], x <- [0..width-1] ]

-- | Create WXCore Color from JuicyPixel pixel
createColor :: PixelRGB8 -- ^  Given JuicyPixel pixel 
			-> Color -- ^ WXCore Color
createColor pixel@(PixelRGB8 r g b) = colorRGB r g b

-- | Convert WXCore image to JuicyPixels image
convertToImageRGB8 :: W.Image () -- ^ Given WXCore image 
				   -> IO (J.Image PixelRGB8) -- ^ JuicyPixel image
convertToImageRGB8 img = do 
	colors <- imageGetPixels img
	width <- getImageWidth img
	height <- getImageHeight img
	return $ prepareImageRGB8 colors width height

-- | Get WXCore image height
getImageHeight :: W.Image () -- ^ Given WXCore image
			   -> IO Int -- ^ Height of image
getImageHeight img = do
	sizer <- get img size
	return $ sizeH $ sizer

-- | Get WXCore image width
getImageWidth :: W.Image () -- ^ Given WXCore image
			  -> IO Int -- ^ Width of image
getImageWidth img = do
	sizer <- get img size 
	return $ sizeW $ sizer

-- | Prepare JuicyPixels image 
prepareImageRGB8 :: [Color] -- ^ WXCore image color values  
				 -> Int -- ^ Width of image
				 -> Int -- ^ Height of image
				 -> J.Image PixelRGB8 -- ^ Created JuicyPixels image
prepareImageRGB8 array width height = generateImage generatorFunction width height where
	generatorFunction :: Int -> Int -> PixelRGB8
	generatorFunction x y = createPixelRGB8  $ array !! (y * width + x)
   
-- | Create JuicyPixels pixel from WXCore Color
createPixelRGB8 :: Color -- ^ Given WXCore Color
			    -> PixelRGB8 -- ^ Created JuicyPixels pixel
createPixelRGB8 color = PixelRGB8 ( fromIntegral $ colorRed color ) ( fromIntegral $ colorGreen color ) ( fromIntegral $ colorBlue color )

-- | Save WXCore image to given filepath location and extension in default png file
saveImage :: W.Image () -- ^ Given WXCore image 
		  -> FilePath -- ^ Given save file location
		  -> IO ()
saveImage img filepath = do
	jimg <- convertToImageRGB8 img
	saveImg jimg (takeExtension filepath) where
		saveImg :: J.Image PixelRGB8 -> String -> IO ()
		saveImg img ".bmp" = saveBmpImage filepath (ImageRGB8 img)
		saveImg img ".jpg" = saveJpgImage 100 filepath (ImageRGB8 img)
		saveImg img  _     = savePngImage filepath (ImageRGB8 img) 