module SymeUI where

import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]
coucou :: (ButtonClass o) => o -> IO ()
coucou b = set b [buttonLabel := "Coucou"]

symeui :: IO ()
symeui = do
  initGUI
  window  <- windowNew
-- Initialization of abstract containers
  hboxMain    <- hBoxNew False 10  -- Main container
  hboxCommand   <- hBoxNew True 10
  hboxMap   <- hBoxNew True 10 -- Box containing the world map image
  vboxData  <- vBoxNew True 10 -- Box containing the data
-- Initialization of icons, images and other fancy features
  pxbufMap <- pixbufNewFromFileAtSize "img/Pughead.png" 400 300
  imgMap <- imageNewFromPixbuf pxbufMap
-- Initialization of command buttons 
  button1 <- buttonNewWithLabel "Bouton 1"
  button2 <- buttonNewWithLabel "Bouton 1"
  button3 <- buttonNewWithLabel "Bouton 1"
  buttonData <- buttonNewWithLabel "Conteneur de données"
  buttonData2 <- buttonNewWithLabel "Conteneur de données"
  buttonData3 <- buttonNewWithLabel "Conteneur de données"
  buttonData4 <- buttonNewWithLabel "Conteneur de données"
  quitButton <- buttonNewWithLabel "Quitter ce programme infernal"
-- Initialization of main window
  set window [windowDefaultWidth := 1024, windowDefaultHeight := 768,
              containerChild := hboxMain , containerBorderWidth := 10]
-- Filling abstract containers with stuff
  boxPackStart hboxMain hboxCommand PackNatural 0
  boxPackStart hboxMain hboxMap PackNatural 0
  boxPackStart hboxMain vboxData PackNatural 0
  boxPackStart hboxCommand button1 PackNatural 0
  boxPackStart hboxCommand button2 PackNatural 0
  boxPackStart hboxCommand quitButton PackNatural 0
  boxPackStart hboxMap imgMap PackNatural 0
  boxPackStart vboxData buttonData PackNatural 5
  boxPackStart vboxData buttonData2 PackNatural 5
  boxPackStart vboxData buttonData3 PackNatural 5
  boxPackStart vboxData buttonData4 PackNatural 5
-- Defining some events
  window `on` deleteEvent $ liftIO mainQuit >> return False
  button1 `on` buttonActivated $ hello button1
  quitButton `on` buttonActivated $ mainQuit
-- Start the window and initiate the GUI for use
  widgetShowAll window
  mainGUI
