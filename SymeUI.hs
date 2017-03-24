import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]
coucou :: (ButtonClass o) => o -> IO ()
coucou b = set b [buttonLabel := "Coucou"]

main :: IO ()
main = do
  initGUI
  window  <- windowNew
  hbox    <- hBoxNew True 10
  hbox2   <- hBoxNew True 10
  hbox3   <- hBoxNew True 10
  button1 <- buttonNewWithLabel "Bouton 1"
  button2 <- buttonNewWithLabel "Bouton 2"
  quitButton <- buttonNewWithLabel "Quitter ce programme infernal"
  set window [windowDefaultWidth := 800, windowDefaultHeight := 600,
              containerChild := hbox , containerBorderWidth := 10]
  boxPackStart hbox hbox2 PackGrow 0
  boxPackStart hbox hbox3 PackGrow 0
  boxPackStart hbox2 button1 PackGrow 0
  boxPackStart hbox3 button2 PackGrow 0 
  boxPackStart hbox3 quitButton PackGrow 0 
  window `on` deleteEvent $ liftIO mainQuit >> return False
  button1 `on` buttonActivated $ hello button1
  button2 `on` buttonActivated $ coucou button2
  quitButton `on` buttonActivated $ mainQuit
  widgetShowAll window
  mainGUI
