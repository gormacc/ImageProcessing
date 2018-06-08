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

-- | Rotate given image with 180 degrees
rotate180 :: Image PixelRGB8 -- ^ Given image
          -> Image PixelRGB8 -- ^ Rotated image
rotate180 img@Image {..} = runST $ do
  -- create mutable image to write new pixels
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

-- | Rotate given image with 90 degrees
rotate90 :: Image PixelRGB8 -- ^ Given image
         -> Image PixelRGB8 -- ^ Rotated image
rotate90 img@Image {..} = runST $ do
  -- create mutable image to write new pixels
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

 -- | Rotate given image with 270 degrees 
rotate270 :: Image PixelRGB8 -- ^ Given image
          -> Image PixelRGB8 -- ^ Rotated image
rotate270 img@Image {..} = runST $ do
  -- create mutable image to write new pixels
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

-- | Scale given image with given values
scale :: Double -- ^ Scale value to X direction
      -> Double -- ^ Scale value to Y direction
      -> Image PixelRGB8 -- ^ Given image
      -> Image PixelRGB8 -- ^ Scaled image
scale scaleX scaleY img@Image {..} = do
  runST $ do
    -- create mutable image to write new pixels
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

-- | Round integer value multiplied with non-integer value
myRound :: Int -- ^ Given integer value
        -> Double -- ^ Double scale value
        -> Int -- ^ Scaled integer value
myRound val sc = truncate $ sc * (fromIntegral val)

-- | Round integer value divided with non-integer value
revertMyRound :: Int -- ^ Given integer value 
              -> Double -- ^ Double scale value
              -> Int -- ^ Scaled integer value
revertMyRound val sc = truncate $ (fromIntegral val) / sc

-- | Convert given image to grey colors
grayscale :: Image PixelRGB8 -- ^ Given image 
          -> Image PixelRGB8 -- ^ Converted image
grayscale = pixelMap grayFunction where
  grayFunction (PixelRGB8 r g b) = PixelRGB8 (toEnum val) (toEnum val) (toEnum val) where
    val = truncate ( ( (toRational r) + (toRational g) + (toRational b)) / 3) 

-- | Limit givent integer value to minimum 0 and maximum 255 value
clamp :: Int -- ^ Given value
      -> Int -- ^ Limited value
clamp val = max 0 $ min 255 val

-- | Brighten given image with given value
brighten :: Int -- ^ Given value
         -> Image PixelRGB8 -- ^ Given image
         -> Image PixelRGB8 -- ^ Brightened image
brighten times = pixelMap brightFunction
      where up v =  (fromEnum v) + times
            brightFunction (PixelRGB8 r g b) =
                    PixelRGB8 (toEnum (clamp (up r))) (toEnum (clamp (up g))) (toEnum (clamp (up b)))

-- | Darken given image with given value
darken :: Int -- ^ Given value 
       -> Image PixelRGB8 -- ^ Given image
       -> Image PixelRGB8 -- ^ Darkened image
darken times = pixelMap darkFunction
      where down v = (fromEnum v ) - times
            darkFunction (PixelRGB8 r g b) =
                    PixelRGB8 (toEnum (clamp (down r))) (toEnum (clamp (down g))) (toEnum(clamp (down b)))

-- | Filter given image with given matrix
imageFilter :: Image PixelRGB8 -- ^ Given image 
            -> Matrix Int -- ^ Given filter matrix
            -> Image PixelRGB8 -- ^ Filtered image
imageFilter img@Image {..} matrix = runST $ do
  -- create mutable image to write new pixels
  mimg <- newMutableImage imageWidth imageHeight 
  let go x y
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg x y (prepareFilterPixel img matrix x y)
            go (x + 1) y
  go 0 0

-- | Apply Filter matrix to given pixel
prepareFilterPixel :: Image PixelRGB8 -- ^ Given pixel
                   -> Matrix Int -- ^ Filter matrix
                   -> Int -- ^ X position of given pixel
                   -> Int -- ^ Y position of given pixel
                   -> PixelRGB8 -- ^ Filtered pixel
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

-- | Retreive value from maybe monda
fromJust :: Maybe PixelRGB8 -- ^ Given pixel in maybe monad 
         -> PixelRGB8 -- ^ Value from maybe monad in default Pixel 0 0 0
fromJust (Just a) = a
fromJust Nothing = pixelZero

-- | Apply filter values to given pixel
preparePixel :: Int -- ^ Sum of the values on red canal
             -> Int -- ^ Sum of the values on green canal
             -> Int -- ^ Sum of the values on blue canal
             -> Int -- ^ Sum of the values on red canal
             -> PixelRGB8 -- ^ New pixel
preparePixel r g b f = PixelRGB8 (preparePixelColor r f) (preparePixelColor g f) (preparePixelColor b f)

-- | Divide color int value with second value
preparePixelColor :: Int -- ^ Given color value in integer
                  -> Int -- ^ Given divider
                  -> Word8 -- ^ Color value in bits
preparePixelColor col f = toEnum $ clamp $ truncate $ ((fromIntegral col) / (fromIntegral f)) 

-- | Pixel with 0 values in colors
pixelZero :: PixelRGB8
pixelZero = PixelRGB8 0 0 0

-- | Retreive pixel from image, if out of bounds returns Nothing
takePixel :: Image PixelRGB8 -- ^ Given image
          -> Int -- ^ X position of pixel
          -> Int -- ^ Y position of pixel
          -> Maybe PixelRGB8 -- ^ Value of pixel in X, Y position
takePixel img@Image {..} x y 
  | x < 0 = Nothing
  | y < 0 = Nothing
  | x >= imageWidth = Nothing
  | y >= imageHeight = Nothing
  | otherwise = Just $ pixelAt img x y

-- | Retreive value from matrix
takeValFromMatrix :: Matrix Int -- ^ Given matrix 
                  -> Int -- ^ Row position of value
                  -> Int -- ^ Column position of value
                  -> Int -- ^ Value being searched
takeValFromMatrix matrix x y = getElem (x+1) (y+1) matrix

-- | Take red canal from pixel
takeRed :: PixelRGB8 -- ^ Given pixel
        -> Int -- ^ Color value in integer
takeRed (PixelRGB8 r g b) = fromEnum r

-- | Take green canal from pixel
takeGreen :: PixelRGB8 -- ^ Given pixel
          -> Int -- ^ Color value in integer
takeGreen (PixelRGB8 r g b) = fromEnum g

-- | Take blue canal from pixel
takeBlue :: PixelRGB8 -- ^ Given pixel
         -> Int -- ^ Color value int integer
takeBlue (PixelRGB8 r g b) = fromEnum b

-- | Increase red canal of pixel
addRedCanal :: Int -- ^ Increase value 
            -> Image PixelRGB8 -- ^ Given pixel 
            -> Image PixelRGB8 -- ^ Increased red canal pixel
addRedCanal times = pixelMap redCanalFunction
      where up v = (fromEnum v) + times
            redCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 (toEnum (clamp (up r))) g b

-- | Decrease red canal of pixel
removeRedCanal :: Int -- ^ Decrease value
               -> Image PixelRGB8 -- ^ Given pixel
               -> Image PixelRGB8 -- ^ Decreased red canal pixel
removeRedCanal times = pixelMap redCanalFunction
      where up v = (fromEnum v) - times
            redCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 (toEnum (clamp (up r))) g b

-- | Increase green canal of pixel
addGreenCanal :: Int -- ^ Increase value 
              -> Image PixelRGB8 -- ^ Given pixel 
              -> Image PixelRGB8 -- ^ Increased green canal pixel
addGreenCanal times = pixelMap greenCanalFunction
      where up v = (fromEnum v) + times
            greenCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 r (toEnum (clamp (up g))) b

-- | Decrease green canal of pixel
removeGreenCanal :: Int -- ^ Decrease value
                 -> Image PixelRGB8 -- ^ Given pixel
                 -> Image PixelRGB8 -- ^ Decreased green canal pixel
removeGreenCanal times = pixelMap greenCanalFunction
      where up v = (fromEnum v) - times
            greenCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 r (toEnum (clamp (up g))) b

-- | Increase blue canal of pixel
addBlueCanal :: Int -- ^ Increase value 
             -> Image PixelRGB8 -- ^ Given pixel 
             -> Image PixelRGB8 -- ^ Increased blue canal pixel
addBlueCanal times = pixelMap blueCanalFunction
      where up v = (fromEnum v) + times
            blueCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 r g (toEnum (clamp (up b)))

-- | Decrease blue canal of pixel             
removeBlueCanal :: Int -- ^ Decrease value
                -> Image PixelRGB8 -- ^ Given pixel
                -> Image PixelRGB8 -- ^ Decreased blue canal pixel
removeBlueCanal times = pixelMap blueCanalFunction
      where up v = (fromEnum v) - times
            blueCanalFunction (PixelRGB8 r g b) = 
                    PixelRGB8 r g (toEnum (clamp (up b)))
