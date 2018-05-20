{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module ImageManipulation where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import qualified Codec.Picture.Types as M

askForData :: String -> IO String
askForData prompt = putStrLn prompt >> getLine >>= return

rotate180 :: Image PixelRGB8 -> Image PixelRGB8
rotate180 img@Image {..} = runST $ do
  mimg <- M.newMutableImage imageWidth imageHeight
  let go x y
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = M.unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg
              (imageWidth - x - 1)
              (imageHeight - y - 1)
              (pixelAt img x y)
            go (x + 1) y
  go 0 0

rotate90 :: Image PixelRGB8 -> Image PixelRGB8
rotate90 img@Image {..} = runST $ do
  mimg <- M.newMutableImage imageHeight imageWidth
  let go x y
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = M.unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg
              (imageHeight - y - 1)
              x
              (pixelAt img x y)
            go (x + 1) y
  go 0 0

rotate270 :: Image PixelRGB8 -> Image PixelRGB8
rotate270 img@Image {..} = runST $ do
  mimg <- M.newMutableImage imageHeight imageWidth
  let go x y
        | x >= imageWidth  = go 0 (y + 1)
        | y >= imageHeight = M.unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg
              y
              (imageWidth - x - 1)
              (pixelAt img x y)
            go (x + 1) y
  go 0 0

myReadImage :: ( Image PixelRGB8 -> Image PixelRGB8 ) -> IO () 
myReadImage fun = do
    path <- askForData "Podaj ścieżkę pliku : "
    path' <- askForData "Podaj ścieżkę pliku wyjściowego : "
    eimg <- readImage path
    case eimg of
        Left err -> putStrLn ("Nie można było odczytać pliku: " ++ err)
        Right (ImageRGB8 img) -> (savePngImage path' . ImageRGB8 . fun) img
        Right dimg ->putStrLn "Możliwa strata jakości obrazu" >> (savePngImage path' . ImageRGB8 . fun . convertRGB8) dimg