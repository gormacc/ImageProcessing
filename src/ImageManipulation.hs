{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module ImageManipulation where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Codec.Picture.Types
import Control.Monad.Primitive

rotate180 :: Image PixelRGB8 -> Image PixelRGB8
rotate180 img@Image {..} = runST $ do
  mimg <- newMutableImage imageWidth imageHeight
  let go x y
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg
              (imageWidth - x - 1)
              (imageHeight - y - 1)
              (pixelAt img x y)
            go (x + 1) y
  go 0 0

rotate90 :: Image PixelRGB8 -> Image PixelRGB8
rotate90 img@Image {..} = runST $ do
  mimg <- newMutableImage imageHeight imageWidth
  let go x y
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg
              (imageHeight - y - 1)
              x
              (pixelAt img x y)
            go (x + 1) y
  go 0 0

rotate270 :: Image PixelRGB8 -> Image PixelRGB8
rotate270 img@Image {..} = runST $ do
  mimg <- newMutableImage imageHeight imageWidth
  let go x y
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg
              y
              (imageWidth - x - 1)
              (pixelAt img x y)
            go (x + 1) y
  go 0 0

scaleX :: Double
scaleX = 2

scaleY :: Double
scaleY = 2

scale :: Image PixelRGB8 -> Image PixelRGB8
scale img@Image {..} = runST $ do
  mimg <- newMutableImage (myRound imageWidth scaleX) (myRound imageHeight scaleY)
  let go x y
        | x >= (myRound imageWidth scaleX)  = go 0 (y + 1)
        | y >= (myRound imageHeight scaleY) = unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg
              x
              y
              (pixelAt img (revertMyRound x scaleX) (revertMyRound y scaleY))
            go (x + 1) y
  go 0 0

myRound :: Int -> Double -> Int
myRound val sc = truncate $ sc * (fromIntegral val)

revertMyRound :: Int -> Double -> Int
revertMyRound val sc = truncate $ (fromIntegral val) / sc

grayscale :: Image PixelRGB8 -> Image PixelRGB8
grayscale = pixelMap grayFunction where
  grayFunction (PixelRGB8 r g b) = PixelRGB8 val val val where
    val = truncate ( (fromIntegral (r + g + b)) / 3) 

brighten :: Image PixelRGB8 -> Image PixelRGB8
brighten = pixelMap brightFunction
      where up v = fromIntegral (fromIntegral v + 1)
            brightFunction (PixelRGB8 r g b) =
                    PixelRGB8 (min (up r)  255) (min (up g) 255) (min (up b) 255)

darken :: Image PixelRGB8 -> Image PixelRGB8
darken = pixelMap darkFunction
      where down v = fromIntegral (fromIntegral v - 1)
            darkFunction (PixelRGB8 r g b) =
                    PixelRGB8 (max (down r)  0) (max (down g) 0) (max (down b) 0)

prepareProgressive :: Image PixelRGB8 -> Int -> IO(Maybe(Image PixelRGB8))
prepareProgressive img@Image {..} partitioner
    | ((partitioner > imageWidth) && (partitioner > imageHeight)) = return Nothing
    | otherwise = do
        stepX <- prepareStep imageWidth partitioner
        stepY <- prepareStep imageHeight partitioner  
        return $ Just $ runST $ do
            mimg <- newMutableImage imageWidth imageHeight
            let go xs ys xe ye
                  | xe > imageWidth  = do
                      prepareSquare mimg (pixelAt img xs ys) xs ys imageWidth ye
                      go 0 ye stepX (ye+stepY)
                  | ye > imageHeight = do
                      prepareSquare mimg (pixelAt img xs ys) xs ys xe imageHeight
                      go xe ys (xe + stepX) imageHeight
                  | xe == imageWidth && ye == imageHeight = unsafeFreezeImage mimg
                  | otherwise = do
                      prepareSquare mimg (pixelAt img xs ys) xs ys xe ye 
                      go xe ys (xe + stepX) ye
            go 0 0 stepX stepY

-- prepareSquare :: PrimMonad m => MutableImage (PrimState m) a -> PixelRGB8 -> Int -> Int -> Int -> Int -> IO ()
prepareSquare mimg pixel xs ys xe ye = do 
  let go x y
        | x >= xe  = go xs (y + 1)
        | y >= ye = return ()
        | otherwise = do
            writePixel mimg x y pixel
            go (x + 1) y
  go xs ys

prepareStep :: Int -> Int -> IO (Int)
prepareStep param part = do return $ truncate $ (fromIntegral param) / (fromIntegral part)
