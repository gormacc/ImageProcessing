{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module ImageManipulation where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Codec.Picture.Types
import Control.Monad.Primitive
import Data.Matrix
import Data.Word

-- | Rotate give image with 180 degrees
rotate180 :: Image PixelRGB8 -- ^ Given image
          -> Image PixelRGB8 -- ^ Rotate image
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

scale :: Double -> Double -> Image PixelRGB8 -> Image PixelRGB8
scale scaleX scaleY img@Image {..} = do
  runST $ do
    mimg <- newMutableImage (myRound imageWidth scaleX) (myRound imageHeight scaleY)
    let newImageWidth = myRound imageWidth scaleX
        newImageHeight = myRound imageHeight scaleY
        go x y
          | x >= newImageWidth  = go 0 (y + 1)
          | y >= newImageHeight = unsafeFreezeImage mimg
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
  grayFunction (PixelRGB8 r g b) = PixelRGB8 (toEnum val) (toEnum val) (toEnum val) where
    val = truncate ( ( (toRational r) + (toRational g) + (toRational b)) / 3) 

clamp :: Int -> Int
clamp val = max 0 $ min 255 val

brighten :: Int -> Image PixelRGB8 -> Image PixelRGB8
brighten times = pixelMap brightFunction
      where up v =  (fromEnum v) + times
            brightFunction (PixelRGB8 r g b) =
                    PixelRGB8 (toEnum (clamp (up r))) (toEnum (clamp (up g))) (toEnum (clamp (up b)))

darken :: Int -> Image PixelRGB8 -> Image PixelRGB8
darken times = pixelMap darkFunction
      where down v = (fromEnum v ) - times
            darkFunction (PixelRGB8 r g b) =
                    PixelRGB8 (toEnum (clamp (down r))) (toEnum (clamp (down g))) (toEnum(clamp (down b)))

prepareStep :: Int -> Int -> IO (Int)
prepareStep param part = do return $ truncate $ (fromIntegral param) / (fromIntegral part)

imageFilter :: Image PixelRGB8 -> Matrix Int -> Image PixelRGB8
imageFilter img@Image {..} matrix = runST $ do
  mimg <- newMutableImage imageWidth imageHeight 
  let go x y
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg x y (prepareFilterPixel img matrix x y)
            go (x + 1) y
  go 0 0

prepareFilterPixel :: Image PixelRGB8 -> Matrix Int -> Int -> Int -> PixelRGB8
prepareFilterPixel img matrix w h = go 0 0 0 0 0 0 where
  go x y sumR sumG sumB sumF
    | x >= 3  = go 0 (y + 1) sumR sumG sumB sumF
    | y >= 3 = preparePixel sumR sumG sumB sumF
    | pixel == Nothing = go (x + 1) y sumR sumG sumB sumF
    | otherwise = go (x + 1) y newSumR newSumG newSumB newSumF  where
      val = takeValFromMatrix matrix x y
      pixel = takePixel img (w + x - 2) (h + y - 2)
      pixelVal = fromJust pixel
      newSumR = sumR + ((takeRed pixelVal) * val)
      newSumG = sumG + ((takeGreen pixelVal) * val)
      newSumB = sumB + ((takeBlue pixelVal) * val)
      newSumF = sumF + (takeValFromMatrix matrix x y)


fromJust :: Maybe PixelRGB8 -> PixelRGB8
fromJust (Just a) = a
fromJust Nothing = pixelZero

preparePixel :: Int -> Int -> Int -> Int -> PixelRGB8
preparePixel r g b f = PixelRGB8 (preparePixelColor r f) (preparePixelColor g f) (preparePixelColor b f)

preparePixelColor :: Int -> Int -> Word8
preparePixelColor col f = toEnum $ clamp $ truncate $ ((fromIntegral col) / (fromIntegral f)) 

pixelZero :: PixelRGB8
pixelZero = PixelRGB8 0 0 0

takePixel :: Image PixelRGB8 -> Int -> Int -> Maybe PixelRGB8
takePixel img@Image {..} x y 
  | x < 0 = Nothing
  | y < 0 = Nothing
  | x >= imageWidth = Nothing
  | y >= imageHeight = Nothing
  | otherwise = Just $ pixelAt img x y

takeValFromMatrix :: Matrix Int -> Int -> Int -> Int
takeValFromMatrix matrix x y = getElem (x+1) (y+1) matrix

takeRed :: PixelRGB8 -> Int
takeRed (PixelRGB8 r g b) = fromEnum r

takeGreen :: PixelRGB8 -> Int
takeGreen (PixelRGB8 r g b) = fromEnum g

takeBlue :: PixelRGB8 -> Int
takeBlue (PixelRGB8 r g b) = fromEnum b

addRedCanal :: Int -> Image PixelRGB8 -> Image PixelRGB8
addRedCanal times = pixelMap redCanalFunction
      where up v = (fromEnum v) + times
            redCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 (toEnum (clamp (up r))) g b

removeRedCanal :: Int -> Image PixelRGB8 -> Image PixelRGB8
removeRedCanal times = pixelMap redCanalFunction
      where up v = (fromEnum v) - times
            redCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 (toEnum (clamp (up r))) g b

addGreenCanal :: Int -> Image PixelRGB8 -> Image PixelRGB8
addGreenCanal times = pixelMap greenCanalFunction
      where up v = (fromEnum v) + times
            greenCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 r (toEnum (clamp (up g))) b

removeGreenCanal :: Int -> Image PixelRGB8 -> Image PixelRGB8
removeGreenCanal times = pixelMap greenCanalFunction
      where up v = (fromEnum v) - times
            greenCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 r (toEnum (clamp (up g))) b

addBlueCanal :: Int -> Image PixelRGB8 -> Image PixelRGB8
addBlueCanal times = pixelMap blueCanalFunction
      where up v = (fromEnum v) + times
            blueCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 r g (toEnum (clamp (up b)))

removeBlueCanal :: Int -> Image PixelRGB8 -> Image PixelRGB8
removeBlueCanal times = pixelMap blueCanalFunction
      where up v = (fromEnum v) - times
            blueCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 r g (toEnum (clamp (up b)))
