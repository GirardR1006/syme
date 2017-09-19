module Main where

import SymeUI

main= do symeui

{--
main :: IO ()
main = do 
    initGUI
    symeUI <- loadSymeui "src/SymeUI.glade"
    mapMeth <- getMapMeth "src/onche.cod"
    initSymeSignals symeUI
    initMainTvw symeUI mapMeth
    mainGUI


--getSymeData :: SymeUI 
--            -> M.Map Int Methode Result of onche.cod parsing
initMainTvw s m = do
    let listofNames = Prelude.map (nom.snd) (M.toList m)
    let listofIDs   = Prelude.map fst (M.toList m)
    let joinedlist = Prelude.zipWith (\a b -> (show(a),b)) listofIDs listofNames
    store <- listStoreNew joinedlist
    cellLayoutSetAttributes (rescolumn s) (celltxt s) store $ \x -> [cellText :=  snd x]
    cellLayoutSetAttributes (idcolumn s) (cellidtxt s) store $ \x -> [cellText :=  fst x]
    treeViewSetModel (restreeview s) store
--}
