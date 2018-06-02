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

       -- create file menu
       file   <- menuPane      [text := "&File"]
       mclose <- menuItem file [text := "&Close\tCtrl+C", help := "Close the image", enabled := False]
       open   <- menuItem file [text := "&Open\tCtrl+O",  help := "Open an image"]
       mir180 <- menuItem file [text := "&Rotate 180", help := "Rotate 180"]
       mir90  <- menuItem file [text := "&Rotate 90", help := "Rotate 90"]
       mir270 <- menuItem file [text := "&Rotate 270", help := "Rotate 270"]
       miscal <- menuItem file [text := "&Scale", help := "Scale"]
       migray <- menuItem file [text := "&Grayscale", help := "Grayscale"]
       mibrig <- menuItem file [text := "&Brighten", help := "Brighten"]
       midark <- menuItem file [text := "&Darken", help := "Darken"]
       mifilt <- menuItem file [text := "&Use filter", help := "Use filter"]
       miprog <- menuItem file [text := "&Progressive", help := "Progressive"]
       menuLine file
       quit   <- menuQuit file [help := "Quit the demo"]

       -- create Help menu
       hlp    <- menuHelp      []
       about  <- menuAbout hlp [help := "About ImageViewer"]

       -- create Toolbar
       tbar       <- toolBar f []
       foimg      <- getDataFileName "bitmaps/fileopen16.png"
       abimg      <- getDataFileName "bitmaps/wxwin16.png"
       tbarOpen   <- toolMenu tbar open  "Open"  foimg []
       tbarAbout  <- toolMenu tbar about "About" abimg []
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
                grid 1 1 [[label "ScaleX:", widget scinx],
                            [label "ScaleY:" , widget sciny ]]
            ]]

        -- stack exec ImageProcessing

       -- set the statusbar, menubar, layout, and add menu item event handlers
       -- note: set the layout before the menubar!
       set f [layout           := row 10 [ column 5 [ vfill $ widget p],
                                           column 5 [ fill $ widget sw]]
             ,statusBar        := [status]
             ,menuBar          := [file,hlp]
             ,outerSize        := sz 800 600    -- niceness

             ]
        
       let networkDescription :: MomentIO ()
           networkDescription = mdo

              eAbout <- event0 tbarAbout command
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

              let showAbout :: IO ()
                  showAbout = infoDialog f "About ImageViewer" "This is a wxHaskell demo"

              reactimate (showAbout <$ eAbout)

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

              let getScaleX :: IO Double
                  getScaleX = do
                    sc <- get scinx text
                    db <- (return (readMaybe sc :: Maybe Double))
                    case db of
                      Nothing -> return 1
                      Just val -> return val
              
              let getScaleY :: IO Double
                  getScaleY = do
                    sc <- get sciny text
                    db <- (return (readMaybe sc :: Maybe Double))
                    case db of
                      Nothing -> return 1
                      Just val -> return val

              let onScale :: IO ()
                  onScale
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             scaleX <- getScaleX
                             scaleY <- getScaleY
                             newImg <- convertToImage $ scale scaleX scaleY img
                             actualizeImage newImg

              reactimate (onScale <$ eScal)

              let onGrayscale :: IO ()
                  onGrayscale
                    = do manipulate grayscale

              reactimate (onGrayscale <$ eGray)

              let onBrighten :: IO ()
                  onBrighten
                    = do manipulate brighten

              reactimate (onBrighten <$ eBrig)

              let onDarken :: IO ()
                  onDarken
                    = do manipulate darken

              reactimate (onDarken <$ eDark)

              let onFilter :: IO ()
                  onFilter
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             newImg <- convertToImage $ imageFilter img testMatrix'
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


