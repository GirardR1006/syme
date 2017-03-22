import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main :: IO ()
main = do
  initGUI
  window <- windowNew
  button <- buttonNew
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerChild := button, containerBorderWidth := 10]
  window `on` deleteEvent $ liftIO mainQuit >> return False
  button `on` buttonActivated $ hello button
  widgetShowAll window
  mainGUI
