module SymeUI where

import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]
coucou :: (ButtonClass o) => o -> IO ()
coucou b = set b [buttonLabel := "Coucou"]

symeui :: IO ()
symeui = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "MockUI.glade"
    window   <- builderGetObject builder castToWindow "mainWindow"
    window `on` deleteEvent $ liftIO mainQuit >> return False
   -- button1 `on` buttonActivated $ hello button1
   -- quitButton `on` buttonActivated $ mainQuit
-- Start the window and initiate the GUI for use
    widgetShowAll window
    mainGUI
