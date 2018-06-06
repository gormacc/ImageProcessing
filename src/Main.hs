{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Exception (onException)
import Graphics.UI.WX 
import Reactive.Banana
import Reactive.Banana.WX
import Reactive.Banana.Frameworks
import Graphics.UI.WXCore as W
import Codec.Picture as J
import Text.Read (readMaybe)
import Control.Monad (liftM)
import Data.Matrix

import Convertion 
import Paths_ImageProcessing
import ImageManipulation

main :: IO ()
main
  = start imageViewer

-- Specify image files for the file open dialog.
imageFiles :: [(String, [String])]
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.gif","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ,("GIF files (*.gif)",["*.gif"])
     ]


-- The image viewer.
imageViewer :: IO ()
imageViewer
  = do -- the main frame, we use 'fullRepaintOnResize' to prevent flicker on resize
       image_ <- getDataFileName "bitmaps/eye.ico"
       f      <- frame [text := "ImageViewer", picture := image_, fullRepaintOnResize := False]

       -- use a mutable variable to hold the image
       vimage <- variable [value := Nothing]

       -- add a scrollable window widget in the frame
       sw     <- scrolledWindow f [scrollRate := sz 10 10, on paint := onPaint vimage
                                  ,bgcolor := white, fullRepaintOnResize := False]

       -- filter matrix
       pa     <- panel f []
       fonon  <- entry pa []
       fontw  <- entry pa []
       fonth  <- entry pa []
       ftwon  <- entry pa []
       ftwtw  <- entry pa []
       ftwth  <- entry pa []
       fthon  <- entry pa []
       fthtw  <- entry pa []
       fthth  <- entry pa []

       -- scale input entries
       p      <- panel f []
       scinx  <- entry p []
       sciny  <- entry p []

       -- darken brighten inputs
       pan    <- panel f []
       darkE  <- entry pan []
       brigE  <- entry pan []

       -- elements of Toolbar
       file   <- menuPane      []
       mclose <- menuItem file []
       open   <- menuItem file []
       mir180 <- menuItem file []
       mir90  <- menuItem file []
       mir270 <- menuItem file []
       miscal <- menuItem file []
       migray <- menuItem file []
       mibrig <- menuItem file []
       midark <- menuItem file []
       mifilt <- menuItem file []
       miprog <- menuItem file []

       -- create Toolbar
       tbar       <- toolBar f []
       foimg      <- getDataFileName "bitmaps/fileopen16.png"
       abimg      <- getDataFileName "bitmaps/wxwin16.png"
       tbarOpen   <- toolMenu tbar open  "Open"  foimg []
       tbarR90    <- toolMenu tbar mir90  "Rotate 90"  abimg []
       tbarR180   <- toolMenu tbar mir180  "Rotate 180"  abimg []
       tbarR270   <- toolMenu tbar mir270  "Rotate 270"  abimg []
       tbarScal   <- toolMenu tbar miscal  "Scale"  abimg []
       tbarGray   <- toolMenu tbar migray  "Grayscale"  abimg []
       tbarBrig   <- toolMenu tbar mibrig  "Brighten"  abimg []
       tbarDark   <- toolMenu tbar midark  "Darken"  abimg []
       tbarFilt   <- toolMenu tbar mifilt  "Use filter" abimg []
       tbarProg   <- toolMenu tbar miprog  "Progressive" abimg []

       -- create statusbar field
       status <- statusField   [text := "Welcome to the wxHaskell ImageViewer"]

       -- set panel for scale inputs

       set p [layout := margin 10 $
            row 1 [
                grid 1 1 [  [label "ScaleX:", widget scinx],
                            [label "ScaleY:", widget sciny]
                         ]]
            ]

       set pan [layout := margin 10 $
            row 1 [
                grid 1 1 [  [label "Darken:", widget darkE],
                            [label "Brighten:", widget brigE]
                          ]]
            ]

       set pa [layout := margin 10 $
          row 1 [
              grid 1 1 [  [label "Image filter:", label "", label ""],
                          [widget fonon, widget fontw, widget fonth],
                          [widget ftwon, widget ftwtw, widget ftwth],
                          [widget fthon, widget fthtw, widget fthth]
                      ]]
          ]

        -- stack exec ImageProcessing

       -- set the statusbar, menubar, layout, and add menu item event handlers
       -- note: set the layout before the menubar!
       set f [layout           := row 10 [ column 5 [ vfill $ column 10 [widget p, widget pan, widget pa]],
                                           column 5 [ fill $ widget sw]
                                         ]
             ,statusBar        := [status]
             ,outerSize        := sz 800 600  
             ]
        
       let networkDescription :: MomentIO ()
           networkDescription = mdo

              eOpen  <- event0 tbarOpen command 
              eR90   <- event0 tbarR90 command
              eR180  <- event0 tbarR180 command
              eR270  <- event0 tbarR270 command
              eScal  <- event0 tbarScal command
              eGray  <- event0 tbarGray command
              eBrig  <- event0 tbarBrig command
              eDark  <- event0 tbarDark command
              eFilt  <- event0 tbarFilt command
              eProg  <- event0 tbarProg command

              let closeImage :: IO ()    
                  closeImage
                      = do mbImage <- swap vimage value Nothing
                           case mbImage of
                             Nothing -> return ()
                             Just im -> objectDelete im >> return () 

              let openImage :: FilePath -> IO ()
                  openImage fname
                      = do -- load the new bitmap
                          im <- imageCreateFromFile fname  -- can fail with exception
                          closeImage
                          set vimage [value := Just im]
                          set mclose [enabled := True]
                          set status [text := fname]
                          -- reset the scrollbars 
                          imsize <- get im size
                          set sw [virtualSize := imsize]
                          repaint sw
                      `onException` repaint sw

              let openClick :: IO ()
                  openClick 
                    = do mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
                         case mbfname of
                           Nothing    -> return ()
                           Just fname -> openImage fname

              reactimate (openClick <$ eOpen)
              
              let getDoubleValue :: TextCtrl () -> IO Double
                  getDoubleValue entr = do
                    ent <- get entr text
                    db <- (return (readMaybe ent :: Maybe Double))
                    case db of
                      Nothing -> return 1
                      Just val -> return val

              let getIntValue :: TextCtrl () -> IO Int
                  getIntValue entr = do
                    ent <- get entr text
                    db <- (return (readMaybe ent :: Maybe Int))
                    case db of
                      Nothing -> return 1
                      Just val -> return val

              let actualizeImage :: W.Image () -> IO ()
                  actualizeImage newImg
                    = do closeImage
                         set vimage [value := Just newImg]
                         imsize <- get newImg size
                         set sw [virtualSize := imsize]
                         repaint sw
              
              let manipulate :: (J.Image PixelRGB8 -> J.Image PixelRGB8) -> IO ()
                  manipulate fun
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             newImg <- convertToImage $ fun img
                             actualizeImage newImg

              let onRotate90 :: IO ()
                  onRotate90
                    = do manipulate rotate90

              reactimate (onRotate90 <$ eR90)

              let onRotate180 :: IO ()
                  onRotate180
                    = do manipulate rotate180

              reactimate (onRotate180 <$ eR180)

              let onRotate270 :: IO ()
                  onRotate270
                    = do manipulate rotate270

              reactimate (onRotate270 <$ eR270)

              let onScale :: IO ()
                  onScale
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             scaleX <- getDoubleValue scinx
                             scaleY <- getDoubleValue sciny
                             newImg <- convertToImage $ scale scaleX scaleY img
                             actualizeImage newImg

              reactimate (onScale <$ eScal)

              let onGrayscale :: IO ()
                  onGrayscale
                    = do manipulate grayscale

              reactimate (onGrayscale <$ eGray)

              let onBrighten :: IO ()
                  onBrighten
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             val <- getIntValue brigE
                             newImg <- convertToImage $ brighten val img
                             actualizeImage newImg

              reactimate (onBrighten <$ eBrig)

              let onDarken :: IO ()
                  onDarken
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             val <- getIntValue darkE
                             newImg <- convertToImage $ darken val img
                             actualizeImage newImg

              reactimate (onDarken <$ eDark)
              
              let readMatrix :: IO(Matrix Int)
                  readMatrix = do
                    onon <- getIntValue fonon
                    ontw <- getIntValue fontw
                    onth <- getIntValue fonth
                    twon <- getIntValue ftwon
                    twtw <- getIntValue ftwtw
                    twth <- getIntValue ftwth
                    thon <- getIntValue fthon
                    thtw <- getIntValue fthtw
                    thth <- getIntValue fthth
                    return $ fromList 3 3 [onon, ontw, onth, 
                                           twon, twtw, twth,
                                           thon, thtw, thth]

              let onFilter :: IO ()
                  onFilter
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             matrix <- readMatrix
                             newImg <- convertToImage $ imageFilter img matrix
                             actualizeImage newImg

              reactimate (onFilter <$ eFilt)
              
              -- let onProgressiveRec :: J.Image PixelRGB8 -> Int -> IO ()
              --     onProgressiveRec img partitioner 
              --       = do retImg <- prepareProgressive img partitioner
              --            case retImg of 
              --              Nothing -> return ()
              --              Just im -> do
              --                wim <- convertToImage im
              --                closeImage
              --                set vimage [value := Just wim]
              --                imsize <- get wim size
              --                set sw [virtualSize := imsize]
              --                repaint sw
              --                onProgressiveRec img (partitioner * 2)

              -- let onProgressive :: IO ()
              --     onProgressive
              --       = do mbImage <- swap vimage value Nothing
              --            case mbImage of
              --              Nothing -> return ()
              --              Just im -> do
              --                img <- convertToImageRGB8 im
              --                onProgressiveRec img 1

              -- reactimate (onProgressive <$ eProg)

       network <- compile networkDescription
       actuate network

       where 
          onPaint vimage dc _viewArea
            = do mbImage <- get vimage value
                 case mbImage of
                   Nothing -> return () 
                   Just im -> drawImage dc im pointZero []


