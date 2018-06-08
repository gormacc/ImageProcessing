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
import Data.Matrix

import Convertion 
import Paths_ImageProcessing
import ImageManipulation

main :: IO ()
main
  = start imageProcesser

-- | Specify image files for the file open and save dialog.
imageFiles :: [(String, [String])]
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ]


-- | Setting all GUI
imageProcesser :: IO ()
imageProcesser
  = do
       -- window frame of the application    
       image_ <- getDataFileName "bitmaps/eye.ico"
       f      <- frame [text := "ImageViewer", picture := image_, fullRepaintOnResize := False]

       -- mutable variable representing shown image
       vimage <- variable [value := Nothing]

       -- enable to scroll our window
       sw     <- scrolledWindow f [scrollRate := sz 10 10, on paint := onPaint vimage
                                  ,bgcolor := white, fullRepaintOnResize := False]

       -- panel containing scale values and button
       p      <- panel f []
       scinx  <- entry p []
       sciny  <- entry p []
       btnSc  <- button p [ text := "Scale" ]

       -- panel containing darken brighten values and buttons
       pan    <- panel f []
       darkE  <- entry pan []
       brigE  <- entry pan []
       btnDa  <- button pan [ text := "Darken" ]
       btnBr  <- button pan [ text := "Brighten" ]

       -- panel containing image filter entries and button
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
       btnFi  <- button pa [ text := "Use filter" ]

       -- panel containing color canals entries and buttons
       pane   <- panel f []
       redE   <- entry pane []
       btnRP  <- button pane [ text := "Up" ]
       btnRM  <- button pane [ text := "Down" ]
       grnE   <- entry pane []
       btnGP  <- button pane [ text := "Up" ]
       btnGM  <- button pane [ text := "Down" ]
       bluE   <- entry pane []
       btnBP  <- button pane [ text := "Up" ]
       btnBM  <- button pane [ text := "Down" ]

       -- creating elements to hold them in toolbar
       file   <- menuPane      []
       mclose <- menuItem file []
       open   <- menuItem file []
       save   <- menuItem file []
       mir180 <- menuItem file []
       mir90  <- menuItem file []
       mir270 <- menuItem file []
       migray <- menuItem file []

       -- creating toolbar elements
       tbar       <- toolBar f []
       foimg      <- getDataFileName "bitmaps/fileopen16.png"
       abimg      <- getDataFileName "bitmaps/wxwin16.png"
       roimg      <- getDataFileName "bitmaps/rotate24.png"
       tbarOpen   <- toolMenu tbar open  "Open"  foimg []
       tbarSave   <- toolMenu tbar save  "Save"  foimg []
       tbarR90    <- toolMenu tbar mir90  "Rotate 90"  roimg []
       tbarR180   <- toolMenu tbar mir180  "Rotate 180"  roimg []
       tbarR270   <- toolMenu tbar mir270  "Rotate 270"  roimg []
       tbarGray   <- toolMenu tbar migray  "Grayscale"  abimg []

       -- create statusbar field
       status <- statusField   [text := "Welcome to the wxHaskell ImageViewer"]

       -- set panel for scale inputs
       set p [layout := margin 10 $
            row 1 [
                grid 1 1 [  [label "ScaleX:", widget scinx],
                            [label "ScaleY:", widget sciny],
                            [widget btnSc]
                         ]]
            ]

       -- set panel for darken and brighten inputs
       set pan [layout := margin 10 $
            column 1 [
                grid 1 1 [  [label "Darken:", widget darkE],
                            [label "Brighten:", widget brigE],
                            [widget btnDa, widget btnBr]
                          ]]
            ]

       -- set panel for image filter inputs
       set pa [layout := margin 10 $
          row 1 [
              grid 1 1 [  [label "Image filter:", label "", label ""],
                          [widget fonon, widget fontw, widget fonth],
                          [widget ftwon, widget ftwtw, widget ftwth],
                          [widget fthon, widget fthtw, widget fthth],
                          [widget btnFi]
                      ]]
          ]

       -- set panel for color canals inputs
       set pane [layout := margin 10 $ 
          row 1 [
              grid 1 1 [  [label "Red canal:", widget redE],
                          [margin 5 $ widget btnRP, margin 5 $ widget btnRM],
                          [label "Green canal:", widget grnE],
                          [margin 5 $ widget btnGP, margin 5 $ widget btnGM],
                          [label "Blue canal:", widget bluE],
                          [margin 5 $ widget btnBP, margin 5 $ widget btnBM]
                        ]]
          ]

       -- set panels, toolbar, status bar in frame
       set f [layout           := row 10 [ column 5 [ vfill $ column 10 [widget p, widget pan, widget pa, widget pane]],
                                           column 5 [ fill $ widget sw]
                                         ]
             ,statusBar        := [status]
             ,outerSize        := sz 800 600  
             ]
     
       -- | Declaring all reactive dependencies
       let networkDescription :: MomentIO ()
           networkDescription = mdo

              -- initializing all reactive event handlers
              eOpen  <- event0 tbarOpen command
              eSave  <- event0 tbarSave command 
              eR90   <- event0 tbarR90  command
              eR180  <- event0 tbarR180 command
              eR270  <- event0 tbarR270 command
              eScal  <- event0 btnSc    command
              eGray  <- event0 tbarGray command
              eBrig  <- event0 btnBr    command
              eDark  <- event0 btnDa    command
              eFilt  <- event0 btnFi    command
              eRedP  <- event0 btnRP    command
              eRedM  <- event0 btnRM    command 
              eGrnP  <- event0 btnGP    command
              eGrnM  <- event0 btnGM    command
              eBluP  <- event0 btnBP    command
              eBluM  <- event0 btnBM    command

                  
              let -- | Deleting current image
                  closeImage :: IO ()    
                  closeImage
                      = do mbImage <- swap vimage value Nothing
                           case mbImage of
                             Nothing -> return ()
                             Just im -> objectDelete im >> return () 
              
                   
              let -- | Setting generated image
                  actualizeImage :: W.Image () -- ^ New image to set
                                 -> IO ()
                  actualizeImage newImg
                      = do closeImage
                           set vimage [value := Just newImg]
                           imsize <- get newImg size
                           set sw [virtualSize := imsize]
                           repaint sw

              let -- | Set image from given filepath
                  openImage :: Prelude.FilePath -> IO ()
                  openImage fname
                      = do im <- imageCreateFromFile fname
                           actualizeImage im
                           set status [text := fname]
                      `onException` repaint sw

              let -- | Run the file open dialog
                  openClick :: IO ()
                  openClick 
                    = do mbfname <- fileOpenDialog f False True "Open image" imageFiles "" ""
                         case mbfname of
                           Nothing    -> return ()
                           Just fname -> openImage fname

              reactimate (openClick <$ eOpen)

              let -- | Saving current image to given filepath location
                  saveVimage :: FilePath -- ^ Filepath given to  
                             -> IO ()
                  saveVimage fname
                      = do mbImage <- swap vimage value Nothing
                           case mbImage of
                             Nothing -> return ()
                             Just im -> saveImage im fname
                          
              let -- | Run the save file dialog
                  saveClick :: IO ()
                  saveClick
                    = do mbfname <- fileSaveDialog f False True "Save image" imageFiles "" ""
                         case mbfname of 
                           Nothing -> return ()
                           Just fname -> saveVimage fname

              reactimate (saveClick <$ eSave)
              
              let -- | Retreiving Double value from text entry, default value is 1
                  getDoubleValue :: TextCtrl () -- ^ Text entry 
                                 -> IO Double -- ^ Double value from entry
                  getDoubleValue entr = do
                    ent <- get entr text
                    db <- (return (readMaybe ent :: Maybe Double))
                    case db of
                      Nothing -> return 1
                      Just val -> return val

              let -- | Retreiving Int value from text entry, default value is 1
                  getIntValue :: TextCtrl () -- ^ Text entry
                              -> IO Int -- ^ Int value from entry
                  getIntValue entr = do
                    ent <- get entr text
                    db <- (return (readMaybe ent :: Maybe Int))
                    case db of
                      Nothing -> return 1
                      Just val -> return val

              let -- | Apply given function to current image
                  manipulate :: (J.Image PixelRGB8 -> J.Image PixelRGB8) -- ^ Function to apply on image
                             -> IO ()
                  manipulate fun
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             newImg <- convertToImage $ fun img
                             actualizeImage newImg

              let -- | Rotate current image over 90 degrees
                  onRotate90 :: IO ()
                  onRotate90
                    = do manipulate rotate90

              reactimate (onRotate90 <$ eR90)

              let -- | Rotate current image over 180 degrees
                  onRotate180 :: IO ()
                  onRotate180
                    = do manipulate rotate180

              reactimate (onRotate180 <$ eR180)

              let -- | Rotate current image over 270 degrees
                  onRotate270 :: IO ()
                  onRotate270
                    = do manipulate rotate270

              reactimate (onRotate270 <$ eR270)

              let -- | Scale current image with values entered in text entries
                  onScale :: IO ()
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

              let -- | Make current image to be in grey
                  onGrayscale :: IO ()
                  onGrayscale
                    = do manipulate grayscale

              reactimate (onGrayscale <$ eGray)

              let -- | Brighten current image with value entered in text entry
                  onBrighten :: IO ()
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

              let -- | Darken current image with value entered in text entry
                  onDarken :: IO ()
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
              
              let -- | Function retreiving matrix from text entries
                  readMatrix :: IO(Matrix Int) -- ^ Return matrix with values from entries, defaults 1 matrix
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

              let -- | Apply filter matrix to current image
                  onFilter :: IO ()
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

              let -- | Increase red canal of the current image with value from text entry
                  onRedUp :: IO ()
                  onRedUp
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             val <- getIntValue redE
                             newImg <- convertToImage $ addRedCanal val img
                             actualizeImage newImg

              reactimate (onRedUp <$ eRedP)

              let -- | Decrease red canal of the current image with value from text entry
                  onRedDown :: IO ()
                  onRedDown
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             val <- getIntValue redE
                             newImg <- convertToImage $ removeRedCanal val img
                             actualizeImage newImg

              reactimate (onRedDown <$ eRedM)

              let -- | Increase green canal of the current image with value from text entry
                  onGreenUp :: IO ()
                  onGreenUp
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             val <- getIntValue grnE
                             newImg <- convertToImage $ addGreenCanal val img
                             actualizeImage newImg

              reactimate (onGreenUp <$ eGrnP)

              let -- | Decrease green canal of the current image with value from text entry
                  onGreenDown :: IO ()
                  onGreenDown
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             val <- getIntValue grnE
                             newImg <- convertToImage $ removeGreenCanal val img
                             actualizeImage newImg

              reactimate (onGreenDown <$ eGrnM)

              let -- | Increase blue canal of the current image with value from text entry
                  onBlueUp :: IO ()
                  onBlueUp
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             val <- getIntValue bluE
                             newImg <- convertToImage $ addBlueCanal val img
                             actualizeImage newImg

              reactimate (onBlueUp <$ eBluP)

              let -- | Decrease blue canal of the current image with value from text entry
                  onBlueDown :: IO ()
                  onBlueDown
                    = do mbImage <- swap vimage value Nothing
                         case mbImage of
                           Nothing -> return ()
                           Just im -> do
                             img <- convertToImageRGB8 im
                             val <- getIntValue bluE
                             newImg <- convertToImage $ removeBlueCanal val img
                             actualizeImage newImg

              reactimate (onBlueDown <$ eBluM)

       -- apply all reactive dependencies to being listened
       network <- compile networkDescription
       actuate network

       where 
          -- | Repaint current image
          onPaint vimage dc _viewArea
            = do mbImage <- get vimage value
                 case mbImage of
                   Nothing -> return () 
                   Just im -> drawImage dc im pointZero []


