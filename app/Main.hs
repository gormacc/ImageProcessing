module Main where

import Control.Exception (onException)
import Graphics.UI.WX 
import Graphics.UI.WXCore 

import CommandHelper

import Paths_ImageProcessing

-- mainLoop :: IO ()
-- mainLoop = do
--     putStrLn "Wpisz komendę..."
--     result <- readCommand
--     if result
--       then mainLoop
--       else return ()

-- main :: IO ()
-- main = mainLoop

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

       -- create file menu
       file   <- menuPane      [text := "&File"]
       mclose <- menuItem file [text := "&Close\tCtrl+C", help := "Close the image", enabled := False]
       open   <- menuItem file [text := "&Open\tCtrl+O",  help := "Open an image"]
       menuLine file
       quit   <- menuQuit file [help := "Quit the demo"]

       -- create Help menu
       hlp    <- menuHelp      []
       about  <- menuAbout hlp [help := "About ImageViewer"]

       -- create Toolbar
       tbar   <- toolBar f []
       foimg  <- getDataFileName "bitmaps/fileopen16.png"
       abimg  <- getDataFileName "bitmaps/wxwin16.png"
       _      <- toolMenu tbar open  "Open"  foimg []
       _      <- toolMenu tbar about "About" abimg []

       -- create statusbar field
       status <- statusField   [text := "Welcome to the wxHaskell ImageViewer"]

       -- set the statusbar, menubar, layout, and add menu item event handlers
       -- note: set the layout before the menubar!
       set f [layout           := column 1 [hfill $ hrule 1  -- add divider between toolbar and scrolledWindow
                                           ,fill (widget sw)]
             ,statusBar        := [status]
             ,menuBar          := [file,hlp]
             ,outerSize        := sz 400 300    -- niceness
             ,on (menu about)  := infoDialog f "About ImageViewer" "This is a wxHaskell demo"
             ,on (menu quit)   := close f
             ,on (menu open)   := onOpen f sw vimage mclose status 
             ,on (menu mclose) := onClose  sw vimage mclose status

             -- nice close down, but no longer necessary as bitmaps are managed automatically.
             ,on closing       :~ \previous -> do{ closeImage vimage; previous }
             ]
  where
    onOpen :: Frame a -> ScrolledWindow b -> Var (Maybe (Image ())) -> MenuItem c -> StatusField -> IO ()
    onOpen f sw vimage mclose status
      = do mbfname <- fileOpenDialog f False {- change current directory -} True "Open image" imageFiles "" ""
           case mbfname of
             Nothing    -> return ()
             Just fname -> openImage sw vimage mclose status fname

    onClose sw vimage mclose status
      = do closeImage vimage
           set mclose [enabled := False]
           set sw     [virtualSize := sz 0 0]
           set status [text := ""]
           repaint sw

    closeImage vimage
      = do mbImage <- swap vimage value Nothing
           case mbImage of
             Nothing -> return ()
             Just im -> objectDelete im

    openImage sw vimage mclose status fname
      = do -- load the new bitmap
           im <- imageCreateFromFile fname  -- can fail with exception
           closeImage vimage
           set vimage [value := Just im]
           set mclose [enabled := True]
           set status [text := fname]
           -- reset the scrollbars 
           imsize <- get im size
           set sw [virtualSize := imsize]
           repaint sw
       `onException` repaint sw

    onPaint vimage dc _viewArea
      = do mbImage <- get vimage value
           case mbImage of
             Nothing -> return () 
             Just im -> drawImage dc im pointZero []
