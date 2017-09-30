module SymeUINew where

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Data.Map as M
import Data.Time
import Data.IORef
import Methodinno
import MethManage

data SymeUI = SymeUI
    {mainWindow::Window, addMDialog::Dialog, modMDialog::Dialog
    ,delMDialog::Dialog, updMDialog::Dialog, difMDialog::Dialog
    ,addMBttn::Button, modMBttn::Button, delMBttn::Button
    ,aupMBttn::Button, quitBttn::Button 
    ,addaddMBttn::Button, addquitBttn::Button
    ,modmodMBttn::Button, modquitBttn::Button
    ,deldelMBttn::Button, delquitBttn::Button
    ,updupdMBttn::Button, updquitBttn::Button
    ,difsaveMBttn::Button, difquitBttn::Button
    ,searchCombo::ComboBox
    ,nametxt::TextView, fldtxt::TextView, orgntxt::TextView
    ,dscptxt::TextView, linktxt::TextView
    ,diffnametxt::TextView, difffldtxt::TextView
    ,difforgntxt::TextView, diffdscptxt::TextView
    ,difflinktxt::TextView
    ,querytxt::Entry
    ,nametxtadd::Entry
    ,fldtxtadd::Entry
    ,orgntxtadd::Entry
    ,dscptxtadd::Entry
    ,linktxtadd::Entry
    ,nametxtmod::Entry
    ,fldtxtmod::Entry
    ,orgntxtmod::Entry
    ,dscptxtmod::Entry
    ,linktxtmod::Entry
    ,restreeview::TreeView, store::ListStore(Int,String)
    ,rescolumn::TreeViewColumn, idcolumn::TreeViewColumn
    ,celltxt::CellRendererText,cellidtxt::CellRendererText
    ,autreeview::TreeView,autstore::ListStore()
    ,sourcecolumn::TreeViewColumn, lastudcolumn::TreeViewColumn
    ,cellsource::CellRendererText,celllastudtxt::CellRendererText
    ,difftreeview::TreeView
    ,omcolumn::TreeViewColumn, nmcolumn::TreeViewColumn, selcolumn::TreeViewColumn
    ,cellom::CellRendererText, cellnm::CellRendererText, celltg::CellRendererToggle
    }

loadSymeui::String -> IO(SymeUI)
loadSymeui guiPath = do
    builder <- builderNew
    builderAddFromFile builder guiPath
    SymeUI <$> builderGetObject builder castToWindow "mainWindow"
           <*> builderGetObject builder castToDialog "addMethDialog"
           <*> builderGetObject builder castToDialog "modMethDialog"
           <*> builderGetObject builder castToDialog "delMethDialog" 
           <*> builderGetObject builder castToDialog "autoUpdateDialog" 
           <*> builderGetObject builder castToDialog "diffDialog" 
           <*> builderGetObject builder castToButton "addBttn"
           <*> builderGetObject builder castToButton "showBttn"
           <*> builderGetObject builder castToButton "delBttn"
           <*> builderGetObject builder castToButton "autBttn"
           <*> builderGetObject builder castToButton "quitBttn"
           <*> builderGetObject builder castToButton "addMethaddBttn"
           <*> builderGetObject builder castToButton "addMethquitBttn"
           <*> builderGetObject builder castToButton "modMethmodBttn"
           <*> builderGetObject builder castToButton "modMethquitBttn"
           <*> builderGetObject builder castToButton "delMethdelBttn"
           <*> builderGetObject builder castToButton "delMethquitBttn"
           <*> builderGetObject builder castToButton "autoUpdateupdateBttn"
           <*> builderGetObject builder castToButton "autoUpdatequitBttn"
           <*> builderGetObject builder castToButton "diffvalselBttn"
           <*> builderGetObject builder castToButton "diffquitBttn"
           <*> builderGetObject builder castToComboBox "searchCbbx"
           <*> builderGetObject builder castToTextView "nameTxt"
           <*> builderGetObject builder castToTextView "fldTxt"
           <*> builderGetObject builder castToTextView "orgnTxt"
           <*> builderGetObject builder castToTextView "dscpTxt"
           <*> builderGetObject builder castToTextView "linkTxt"
           <*> builderGetObject builder castToTextView "diffnameTxt"
           <*> builderGetObject builder castToTextView "difffldTxt"
           <*> builderGetObject builder castToTextView "difforgnTxt"
           <*> builderGetObject builder castToTextView "diffdscpTxt"
           <*> builderGetObject builder castToTextView "difflinkTxt"
           <*> builderGetObject builder castToEntry "searchTxt" 
           <*> builderGetObject builder castToEntry "addMethnameTxt" 
           <*> builderGetObject builder castToEntry "addMethfldTxt" 
           <*> builderGetObject builder castToEntry "addMethorgnTxt" 
           <*> builderGetObject builder castToEntry "addMethdscpTxt" 
           <*> builderGetObject builder castToEntry "addMethlinkTxt" 
           <*> builderGetObject builder castToEntry "modMethnameTxt" 
           <*> builderGetObject builder castToEntry "modMethfldTxt" 
           <*> builderGetObject builder castToEntry "modMethorgnTxt" 
           <*> builderGetObject builder castToEntry "modMethdscpTxt" 
           <*> builderGetObject builder castToEntry "modMethlinkTxt"
           <*> builderGetObject builder castToTreeView "resultTreeView"
           <*> builderGetObject builder castToTreeViewColumn "resultTreeViewColumn"
           <*> builderGetObject builder castToTreeViewColumn "resultIDTreeViewColumn"
           <*> builderGetObject builder castToCellRendererText "resultTreeViewCell"
           <*> builderGetObject builder castToCellRendererText "resultIDTreeViewCell"
----Auto-update selector treeview, used to show the different sources of online
----data and the last date they were fetched 
           <*> builderGetObject builder castToTreeView "autoUpdateTreeView"
           <*> builderGetObject builder castToTreeViewColumn "sourceTreeViewColumn"
           <*> builderGetObject builder castToTreeViewColumn "lastUDTreeViewColumn"
           <*> builderGetObject builder castToCellRendererText "sourceTreeViewCell"
           <*> builderGetObject builder castToCellRendererText "lastUDTreeViewCell"
----Diff treeview, used to show the result of a symebot update on a source
           <*> builderGetObject builder castToTreeView "diffDisplayTreeView"
           <*> builderGetObject builder castToTreeViewColumn "oldMethTreeViewColumn"
           <*> builderGetObject builder castToTreeViewColumn "newMethTreeViewColumn"
           <*> builderGetObject builder castToTreeViewColumn "toggleTreeViewColumn"
           <*> builderGetObject builder castToCellRendererText "oldMethTreeViewCell"
           <*> builderGetObject builder castToCellRendererText "newMethTreeViewCell"
           <*> builderGetObject builder castToCellRendererToggle "toggleTreeViewCell"

initSymeSignals s = do
    (mainWindow s) `on` deleteEvent $ liftIO (mainQuit) >> return False 
    (addMDialog s) `on` deleteEvent $ liftIO (widgetHide (addMDialog s)) >> return False
    (modMDialog s) `on` deleteEvent $ liftIO (widgetHide (modMDialog s)) >> return False
    (delMDialog s) `on` deleteEvent $ liftIO (widgetHide (delMDialog s)) >> return False  
    (updMDialog s) `on` deleteEvent $ liftIO (widgetHide (updMDialog s)) >> return False
    (difMDialog s) `on` deleteEvent $ liftIO (widgetHide (difMDialog s)) >> return False

    (addMBttn s) `on` buttonActivated $ do 
        handleAdd <- dialogRun (addMDialog s)
        if handleAdd == ResponseOk 
            then putStrLn "Ok signal received, adding record."  
            >> widgetHide (addMDialog s)
        else
            putStrLn "Cancel signal received, hiding window."   
            >> widgetHide (addMDialog s)

    (modMBttn s) `on` buttonActivated $ do 
        handleMod <- dialogRun (modMDialog s)
        if handleMod == ResponseOk 
            then putStrLn "Ok signal received, modifying record." 
            >> widgetHide (modMDialog s)
        else
            putStrLn "Cancel signal received, hiding window."
            >> widgetHide (modMDialog s)

    (delMBttn s) `on` buttonActivated $ do 
        handleDel <- dialogRun (delMDialog s)
        if handleDel == ResponseOk
            then putStrLn "Ok signal received, deleting record."
            >> widgetHide (delMDialog s)
        else
            putStrLn "Cancel signal received, hiding window."
            >> widgetHide (delMDialog s)
                                    
    (aupMBttn s) `on` buttonActivated $ do 
        handleUpd <- dialogRun (updMDialog s)
        if handleUpd == ResponseOk
            then putStrLn "Ok signal received, auto-update complete."
            >> widgetHide (updMDialog s)
        else
            putStrLn "Cancel signal received, hiding window."
            >> widgetHide (updMDialog s)

initSymeSignalsWithData g m s = do
    (quitBttn s) `on` buttonActivated $ do 
        putStrLn "Saving data in storage..."
        saveMapInFile m "src/onche.cod"
        putStrLn "Quitting project Syme..."
        mainQuit

----TreeView signals
    restvs <- treeViewGetSelection (restreeview s)
    restvs `on` treeSelectionSelectionChanged $ do 
        id <- currentTvwSelectionID (restreeview s) store
        ref <- readIORef (m)
        if id == -1 
            then return()
        else 
            case M.lookup id ref of Just r  -> textBufferSetText namebuffer (nom r)
                                                >>textBufferSetText fldbuffer (domaine r)
                                                >>textBufferSetText orgnbuffer (origine r)
                                                >>textBufferSetText dscpbuffer (description r)
                                                >>textBufferSetText linkbuffer (lien r)
                                    Nothing -> return()
---Update the treeview content 
    querytxt `on` editableChanged $ do ref <- readIORef (m)
                                       updateStore scbbx (querytxt s) ref store      

--currentTvwSelectionID :: TreeView -> ListStore (String,String) -> IO(Int)
--currentTvwSelectionID t m = do
--                        --TODO: take the model directly from the treeView
--                        --Make it robust against empty selection exception
--                        --tvwM <- treeViewGetModel t 
--                        tvwS <- treeViewGetSelection t
--                        tvwP <- treeSelectionGetSelectedRows tvwS
--                        if tvwP == []
--                            then return(-1)
--                            else do let s = Prelude.head (Prelude.head tvwP)
--                                    v <- listStoreGetValue m s
--                                    putStrLn $ "selected item: " ++ (snd v) ++ " with ID " ++ (fst v)
--                                    return(read(fst v)::Int)
--
--currentTvwSelectionURL :: TreeView -> ListStore (String,Day) -> IO(String)
--currentTvwSelectionURL t m = do tvwS <- treeViewGetSelection t
--                                tvwP <- treeSelectionGetSelectedRows tvwS
--                                if tvwP == []
--                                    then return("NO_URL")
--                                    else do let s = Prelude.head (Prelude.head tvwP)
--                                            v <- listStoreGetValue m s
--                                            putStrLn $ "selected url: " ++ fst(v)
--                                            return $ fst v
--
--
--ded :: TreeView -> ListStore (RawO,Bool) -> IO(RawO)
--ded t m = do tvwS <- treeViewGetSelection t
--             tvwP <- treeSelectionGetSelectedRows tvwS
--             if tvwP == []
--                 then return(RawO{titlep=""})
--             else
--                 do let s = Prelude.head (Prelude.head tvwP)
--                    v <- listStoreGetValue m s
--                    return $ fst v

