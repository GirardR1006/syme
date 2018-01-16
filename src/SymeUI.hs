{-#LANGUAGE ScopedTypeVariables#-}

{--
    GUI handling.
    A FAIRE: Le programme compile, mais il y a des soucis dans l'enregistrement. 
    Les noms sont mal enregistrés et les données ne semblent pas lues en entier.
--}

module SymeUI where

import Control.Monad.Trans(liftIO)
import Data.Aeson
import Data.IORef
import Graphics.UI.Gtk
import Data.Text as T
import Data.Maybe
import Data.Map as M
import Data.List 
import Data.HashMap.Lazy as H
import Data.ByteString.Lazy.UTF8 as L
import Data.Time
import MethManage
import SourceManage
import Methodinno
import Symebot

assignComboTextToBuffer :: (ComboBoxClass o, TextBufferClass i) => o -> i -> IO()
assignComboTextToBuffer o i = do
            tmp <- comboBoxGetActiveText o
            case tmp of Just str -> textBufferSetText i (T.unpack str)      
                        Nothing  -> textBufferSetText i ""

registerMeth :: EntryClass a => a -> a -> a -> a -> a -> IO(Methode)
registerMeth nameE fldE orgnE dscpE linkE = do
                                            name <- entryGetText nameE
                                            fld <- entryGetText fldE
                                            orgn <- entryGetText orgnE
                                            dscp <- entryGetText dscpE
                                            link <- entryGetText linkE
                                            putStrLn ("name: " ++ name)
                                            putStrLn ("field: " ++ fld)
                                            putStrLn ("link: " ++ link)
                                            putStrLn ("description: " ++ dscp)
                                            return( Methode {nom=name
                                                            ,domaine=fld
                                                            ,origine=orgn
                                                            ,description=dscp
                                                            ,lien=link
                                                            ,sourceName="utilisateur"} )

currentTvwSelection :: TreeView -> ListStore (a) -> IO(Maybe(a))
currentTvwSelection t m = do
                        tvwS <- treeViewGetSelection t
                        tvwP <- treeSelectionGetSelectedRows tvwS
                        if tvwP == [] 
                            then return(Nothing)
                        else 
                            do let s = Prelude.head (Prelude.head tvwP)
                               v <- listStoreGetValue m s
                               return (Just(v))

toggleStuff t m = do
    tvwS <- treeViewGetSelection t
    tvwP <- treeSelectionGetSelectedRows tvwS
    if tvwP == [] 
        then return()
    else 
        do let s = Prelude.head (Prelude.head tvwP)
           v <- listStoreGetValue m s
           listStoreSetValue m s (fst v, not (snd v))

currentTvwSelectionID :: TreeView -> ListStore (String,String) -> IO(Int) 
currentTvwSelectionID t m = do
                        --TODO: take the model directly from the treeView
                        --Make it robust against empty selection exception
                        --tvwM <- treeViewGetModel t 
                        tvwS <- treeViewGetSelection t
                        tvwP <- treeSelectionGetSelectedRows tvwS
                        if tvwP == [] 
                            then return(-1)
                            else do let s = Prelude.head (Prelude.head tvwP)
                                    v <- listStoreGetValue m s
                                    putStrLn $ "selected item: " ++ (snd v) ++ " with ID " ++ (fst v)
                                    return(read(fst v)::Int)

currentTvwSelectionURL :: TreeView -> ListStore (String,Day) -> IO(String) 
currentTvwSelectionURL t m = do tvwS <- treeViewGetSelection t
                                tvwP <- treeSelectionGetSelectedRows tvwS
                                if tvwP == [] 
                                    then return("NO_URL")
                                    else do let s = Prelude.head (Prelude.head tvwP)
                                            v <- listStoreGetValue m s
                                            putStrLn $ "selected url: " ++ fst(v)
                                            return $ fst v


ded :: TreeView -> ListStore (RawO,Bool) -> IO(RawO) 
ded t m = do tvwS <- treeViewGetSelection t
             tvwP <- treeSelectionGetSelectedRows tvwS
             if tvwP == [] 
                 then return(defO)
             else 
                 do let s = Prelude.head (Prelude.head tvwP)
                    v <- listStoreGetValue m s
                    return $ fst v

convertMapToListStore :: M.Map Int Methode -> [(String,String)]
convertMapToListStore m = Prelude.map f (M.toList m)
    where f (a,b) = (show a,nom b)
convertOutToListStore :: [RawO] -> [(RawO,Bool)]
convertOutToListStore m = Prelude.map f m
    where f m = (m,True)
----Display the search result of query inside of a treeview
mapBySearchResult :: ComboBox -> Entry -> M.Map Int Methode -> IO(Maybe ([(String,String)]))
mapBySearchResult c e m  = do
                         query <- entryGetText e
                         putStrLn query
                         criterion <- comboBoxGetActiveText c
                         let rmap = case criterion of Just  str -> searchBy (T.unpack str) query m  
                                                      Nothing   -> Nothing
                         return (convertMapToListStore <$> rmap)

--Update store content regarding to search query
updateStore :: ComboBox -> Entry -> M.Map Int Methode -> ListStore (String,String) -> IO()
updateStore c e m l = do res <- mapBySearchResult c e m
                         print res
                         case res of Just []      -> listStoreClear l
                                     Just x       -> do listStoreClear l >> mapM_ (listStoreAppend l) x 
                                     Nothing      -> return()
--Update store content after auto-update
updateStoreAfterUpdate :: M.Map Int Methode -> ListStore (String,String) -> IO()
updateStoreAfterUpdate m l = do let listStoreFromMap = convertMapToListStore m
                                case listStoreFromMap of []      -> listStoreClear l
                                                         x       -> do listStoreClear l >> mapM_ (listStoreAppend l) x 
symeui :: IO ()
symeui = do
    initGUI
-- Loading the static interface definition from external file generated by Glade
-- UI constructor
    builder <- builderNew
    builderAddFromFile builder "./src/SymeUI.glade"
--Windows and dialog definition
    window   <- builderGetObject builder castToWindow "mainWindow"
    dialogadd <- builderGetObject builder castToDialog "addMethDialog"
    dialogmod <- builderGetObject builder castToDialog "modMethDialog"
    dialogdel <- builderGetObject builder castToDialog "delMethDialog" 
    dialogupd <- builderGetObject builder castToDialog "autoUpdateDialog" 
    dialogdif <- builderGetObject builder castToDialog "diffDialog" 
    dialogern <- builderGetObject builder castToDialog "errorNetworkDialog" 
    window `on` deleteEvent $ liftIO mainQuit >> return False
    dialogadd `on` deleteEvent $ liftIO (widgetHide dialogadd) >> return False
    dialogmod `on` deleteEvent $ liftIO (widgetHide dialogmod) >> return False
    dialogdel `on` deleteEvent $ liftIO (widgetHide dialogdel) >> return False
    dialogupd `on` deleteEvent $ liftIO (widgetHide dialogupd) >> return False
    dialogdif `on` deleteEvent $ liftIO (widgetHide dialogdif) >> return False
    dialogern `on` deleteEvent $ liftIO (widgetHide dialogern) >> return False
--Button definition
    abttn <- builderGetObject builder castToButton "addBttn"
    sbttn <- builderGetObject builder castToButton "showBttn"
    dbttn <- builderGetObject builder castToButton "delBttn"
    autbttn<- builderGetObject builder castToButton "autBttn"
    qbttn <- builderGetObject builder castToButton "quitBttn"
    csvbttn <- builderGetObject builder castToButton "csvBttn"
    abttnadd <- builderGetObject builder castToButton "addMethaddBttn"
    qbttnadd <- builderGetObject builder castToButton "addMethquitBttn"
    mbttnmod <- builderGetObject builder castToButton "modMethmodBttn"
    qbttnmod <- builderGetObject builder castToButton "modMethquitBttn"
    dbttndel <- builderGetObject builder castToButton "delMethdelBttn"
    qbttndel <- builderGetObject builder castToButton "delMethquitBttn"
    abttnau <- builderGetObject builder castToButton "autoUpdateupdateBttn"
    qbttnau <- builderGetObject builder castToButton "autoUpdatequitBttn"
    fbttnau <- builderGetObject builder castToButton "autoUpdatefulluBttn"
    vsbttndiff <- builderGetObject builder castToButton "diffvalselBttn"
    qbttndiff <- builderGetObject builder castToButton "diffquitBttn"
    qbttnerrn <- builderGetObject builder castToButton "errorNetworkDialogOkBttn"
--Comboboxes definition
--Search combobox item must match with the searchBy function criteria
    scbbx <- builderGetObject builder castToComboBox "searchCbbx"
    comboBoxSetModelText scbbx
    comboBoxAppendText scbbx $ T.pack "Nom" 
    comboBoxAppendText scbbx $ T.pack "Domaine d'application" 
    comboBoxAppendText scbbx $ T.pack "Origine" 
    comboBoxAppendText scbbx $ T.pack "Description" 
--Textdisplay definition
    nametxt <- builderGetObject builder castToTextView "nameTxt"
    fldtxt <- builderGetObject builder castToTextView "fldTxt"
    orgntxt <- builderGetObject builder castToTextView "orgnTxt"
    dscptxt <- builderGetObject builder castToTextView "dscpTxt"
    linktxt <- builderGetObject builder castToTextView "linkTxt"
    diffnametxt <- builderGetObject builder castToTextView "diffnameTxt"
    difffldtxt <- builderGetObject builder castToTextView "difffldTxt"
    difforgntxt <- builderGetObject builder castToTextView "difforgnTxt"
    diffdscptxt <- builderGetObject builder castToTextView "diffdscpTxt"
    difflinktxt <- builderGetObject builder castToTextView "difflinkTxt"
--Textbuffers definition and assignation
    namebuffer <- textBufferNew Nothing
    fldbuffer <- textBufferNew Nothing
    orgnbuffer <- textBufferNew Nothing
    dscpbuffer <- textBufferNew Nothing
    linkbuffer <- textBufferNew Nothing
    diffnamebuffer <- textBufferNew Nothing
    difffldbuffer <- textBufferNew Nothing
    difforgnbuffer <- textBufferNew Nothing
    diffdscpbuffer <- textBufferNew Nothing
    difflinkbuffer <- textBufferNew Nothing
    textViewSetBuffer nametxt namebuffer
    textViewSetBuffer fldtxt fldbuffer
    textViewSetBuffer orgntxt orgnbuffer
    textViewSetBuffer dscptxt dscpbuffer
    textViewSetBuffer linktxt linkbuffer
    textViewSetBuffer diffnametxt diffnamebuffer
    textViewSetBuffer difffldtxt difffldbuffer
    textViewSetBuffer difforgntxt difforgnbuffer
    textViewSetBuffer diffdscptxt diffdscpbuffer
    textViewSetBuffer difflinktxt difflinkbuffer
--Text entries definition
    querytxt <- builderGetObject builder castToEntry "searchTxt"
    nametxtadd <- builderGetObject builder castToEntry "addMethnameTxt"
    fldtxtadd <- builderGetObject builder castToEntry "addMethfldTxt"
    orgntxtadd <- builderGetObject builder castToEntry "addMethorgnTxt"
    dscptxtadd <- builderGetObject builder castToEntry "addMethdscpTxt"
    linktxtadd <- builderGetObject builder castToEntry "addMethlinkTxt"
    nametxtmod <- builderGetObject builder castToEntry "modMethnameTxt"
    fldtxtmod <- builderGetObject builder castToEntry "modMethfldTxt"
    orgntxtmod <- builderGetObject builder castToEntry "modMethorgnTxt"
    dscptxtmod <- builderGetObject builder castToEntry "modMethdscpTxt"
    linktxtmod <- builderGetObject builder castToEntry "modMethlinkTxt"
--Loading data from file. Since it will be modified several time during
--runtime (on a single-thread basis), IORef monad is used.
    nm <- getMapMeth "src/onche.cod"
    si <- sourcesFromFile "src/.sources"
    methmap<- newIORef nm
    --TODO: fonction qui créé de si et nm une liste de valeurs Source 
--TreeView initialization and configuration
----Main treeview
    restreeview <- builderGetObject builder castToTreeView "resultTreeView"
    restreeviewselect <- treeViewGetSelection restreeview
    rescolumn <- builderGetObject builder castToTreeViewColumn "resultTreeViewColumn"
    idcolumn <- builderGetObject builder castToTreeViewColumn "resultIDTreeViewColumn"
    celltxt <- builderGetObject builder castToCellRendererText "resultTreeViewCell"
    cellidtxt <- builderGetObject builder castToCellRendererText "resultIDTreeViewCell"
    let listofNames = Prelude.map (nom.snd) (M.toList nm)
    let listofIDs   = Prelude.map fst (M.toList nm)
    let joinedlist = Prelude.zipWith (\a b -> (show(a),b)) listofIDs listofNames
    store <- listStoreNew joinedlist
    cellLayoutSetAttributes rescolumn celltxt store $ \x -> [cellText :=  snd x]
    cellLayoutSetAttributes idcolumn cellidtxt store $ \x -> [cellText :=  fst x]
    treeViewSetModel restreeview store
----Auto-update selector treeview, used to show the different sources of online
----data and the last date they were fetched 
    autreeview <- builderGetObject builder castToTreeView "autoUpdateTreeView"
    autreeviewselect <- treeViewGetSelection autreeview
    sourcecolumn <- builderGetObject builder castToTreeViewColumn "sourceTreeViewColumn"
    lastudcolumn <- builderGetObject builder castToTreeViewColumn "lastUDTreeViewColumn"
    cellsource <- builderGetObject builder castToCellRendererText "sourceTreeViewCell"
    celllastud <- builderGetObject builder castToCellRendererText "lastUDTreeViewCell"
    audstore <- listStoreNew si
    cellLayoutSetAttributes sourcecolumn cellsource audstore $ \x -> [cellText :=  fst x]
    cellLayoutSetAttributes lastudcolumn celllastud audstore $ \x -> [cellText :=  show(snd x)]
    treeViewSetModel autreeview audstore
----Diff treeview, used to show the result of a symebot update on a source
    difftreeview <- builderGetObject builder castToTreeView "diffDisplayTreeView"
    difftreeviewselect <- treeViewGetSelection difftreeview
    omcolumn <- builderGetObject builder castToTreeViewColumn "oldMethTreeViewColumn"
    selcolumn <- builderGetObject builder castToTreeViewColumn "toggleTreeViewColumn"
    cellom <- builderGetObject builder castToCellRendererText "oldMethTreeViewCell"
    celltg <- builderGetObject builder castToCellRendererToggle "toggleTreeViewCell"
    difstore <- listStoreNew [(defO,True)]
    cellLayoutSetAttributes omcolumn cellom difstore $ \x -> [cellText :=  (titlep.fst $ x)]
    cellLayoutSetAttributes selcolumn celltg difstore $ \x -> [cellToggleActive :=  snd(x)]
    treeViewSetModel difftreeview difstore

-- Linking functions to signals
-- Note that the only relevant signals here are those defined
-- in the Gtk package documentation on Hackage. 
-- Handlers defined in Glade are of no use here.

--Main window signals
----Buttons
    abttn `on` buttonActivated $ do handleAdd <- dialogRun dialogadd
                                    --TODO: find a more efficient way to handle
                                    --dialog than simply hide it
                                    if handleAdd == ResponseOk 
                                        then putStrLn "Ok signal received, adding record."
                                        >> widgetHide dialogadd
                                    else
                                        putStrLn "Cancel signal received, hiding window."
                                        >> widgetHide dialogadd

    sbttn `on` buttonActivated $ do handleMod <- dialogRun dialogmod
                                    if handleMod == ResponseOk 
                                        then putStrLn "Ok signal received, modifying record."
                                        >> widgetHide dialogmod
                                    else
                                        putStrLn "Cancel signal received, hiding window."
                                        >> widgetHide dialogmod
    dbttn `on` buttonActivated $ do handleDel <- dialogRun dialogdel
                                    if handleDel == ResponseOk
                                        then putStrLn "Ok signal received, deleting record."
                                        >> widgetHide dialogdel
                                    else
                                        putStrLn "Cancel signal received, hiding window."
                                        >> widgetHide dialogdel
                                    
    autbttn `on` buttonActivated $ do handleUpd <- dialogRun dialogupd
                                      if handleUpd == ResponseOk
                                          then putStrLn "Ok signal received, auto-update complete."
                                          >> widgetHide dialogupd
                                      else
                                          putStrLn "Cancel signal received, hiding window."
                                          >> widgetHide dialogupd
                                    
    csvbttn `on` buttonActivated $ do putStrLn "Saving data in storage..."
                                      rf <- readIORef methmap
                                      today <- curDate
                                      saveMapInFile rf ("src/"++show(today)++".csv")
                                      putStrLn "Data saved"
    qbttn `on` buttonActivated $ do putStrLn "Saving data in storage..."
                                    rf <- readIORef methmap
                                    saveMapInFile rf "src/onche.cod"
                                    putStrLn "Quitting project Syme..."
                                    mainQuit
----TreeView signals
    restreeviewselect `on` treeSelectionSelectionChanged $ do id <- currentTvwSelectionID restreeview store
                                                              ref <- readIORef methmap
                                                              if id == -1 
                                                                  then return()
                                                              else case M.lookup id ref of Just r  -> textBufferSetText namebuffer (nom r)>>textBufferSetText fldbuffer (domaine r)>>textBufferSetText orgnbuffer (origine r)>>textBufferSetText dscpbuffer (description r)>>textBufferSetText linkbuffer (lien r)
                                                                                           Nothing -> return()
---Update the treeview content 
    querytxt `on` editableChanged $ do ref <- readIORef methmap
                                       updateStore scbbx querytxt ref store      
--Add method window signals
----Buttons
    abttnadd `on` buttonActivated $ do newmeth <- registerMeth nametxtadd fldtxtadd orgntxtadd dscptxtadd linktxtadd
                                       ref <- readIORef methmap
                                       putStrLn (show newmeth)
                                       listStoreAppend store (show((maxKeyInMap ref) + 1),(nom newmeth))
                                       atomicModifyIORef methmap (\m->(addMethToMap newmeth m,()))  
                                       print <$> readIORef methmap
                                       dialogResponse dialogadd ResponseOk

    qbttnadd `on` buttonActivated $ dialogResponse dialogadd ResponseCancel
--Modify method window signals
----Buttons
    dialogmod `on` showSignal $ do id <- currentTvwSelectionID restreeview store
                                   ref <- readIORef methmap
                                   if id == -1
                                       then return()
                                   else case M.lookup id ref of Just r  -> entrySetText nametxtmod (nom r)>>entrySetText fldtxtmod (domaine r)>>entrySetText orgntxtmod (origine r)>>entrySetText dscptxtmod (description r)>>entrySetText linktxtmod (lien r)
                                                                Nothing -> return()
    mbttnmod `on` buttonActivated $ do id <- currentTvwSelectionID restreeview store
                                       ref <- readIORef methmap
                                       modmeth <- registerMeth nametxtmod fldtxtmod orgntxtmod dscptxtmod linktxtmod
                                       listStoreSetValue store id (show(id),nom modmeth)
                                       atomicModifyIORef methmap (\m->(M.insert id modmeth m,())) 
                                    
                                       dialogResponse dialogmod ResponseOk


    qbttnmod `on` buttonActivated $ do dialogResponse dialogmod ResponseCancel
--Delete method window signals
----Buttons
    dbttndel `on` buttonActivated $ do id <- currentTvwSelectionID restreeview store
                                       if id == -1 
                                           then dialogResponse dialogdel ResponseOk
                                       else do listStoreRemove store (id -1)
                                               atomicModifyIORef methmap (\ m -> (M.delete id m,())) 
                                               print <$> readIORef methmap
                                               dialogResponse dialogdel ResponseOk 
    qbttndel `on` buttonActivated $ dialogResponse dialogdel ResponseCancel
--Auto-update method window signals
--Need to revamp this: 
--User should only have to push a button and update every source
--
----Buttons
    abttnau `on` buttonActivated $ do url <- currentTvwSelectionURL autreeview audstore
                                      if url == "NO_URL"
                                               then dialogResponse dialogdif ResponseOk
                                      else do parseres <- symeScrape ["url",url]
                                              let a = convertOutToListStore parseres
                                              listStoreClear difstore
                                              listStoreAppend difstore (defO,False)
                                              mapM_ (listStoreAppend difstore) a  
                                              handleDiff <- dialogRun dialogdif 
                                              if handleDiff == ResponseOk
                                                  then putStrLn "Ok signal received, auto-update complete." 
                                                  >> widgetHide dialogdif
                                              else 
                                                  putStrLn "Cancel signal received, hiding window"
                                                  >> widgetHide dialogdif
    qbttnau `on` buttonActivated $ dialogResponse dialogupd ResponseCancel
    fbttnau `on` buttonActivated $ do let urls = Prelude.map fst si
                                      results <- symeScrape' urls
                                      case results of 
                                        [] -> do handleNetError <- dialogRun dialogern
                                                 if handleNetError == ResponseOk then do
                                                   widgetHide dialogern 
                                                 else do
                                                   widgetHide dialogern
                                                 dialogResponse dialogupd ResponseOk
                                        _ -> do let listOfMeth = pleaseKillMe urls results
                                                snort listOfMeth methmap
                                                ref <- readIORef methmap
                                                updateStoreAfterUpdate ref store      
                                                dialogResponse dialogupd ResponseOk
--Diff window signals
----Buttons
    --Get the proposal that were ticked by the user, then register them in
    --methref
    vsbttndiff `on` buttonActivated $ do url <- currentTvwSelectionURL autreeview audstore
                                         dsl <- listStoreToList difstore
                                         let tobs = Prelude.filter(\x->snd(x)==True) dsl
                                         let nml = Prelude.map ((cmfo url).fst) tobs
                                         chainAtomIO methmap nml
                                         dialogResponse dialogdif ResponseOk

    qbttndiff `on` buttonActivated $ dialogResponse dialogdif ResponseCancel
--Network error window signal
----Button
    qbttnerrn `on` buttonActivated $ dialogResponse dialogern ResponseOk
----TreeView
    difftreeviewselect `on` treeSelectionSelectionChanged $ do 
        o <- ded difftreeview difstore
        textBufferSetText diffnamebuffer (titlep o)
        textBufferSetText difffldbuffer (domainp o)                                      
        textBufferSetText difforgnbuffer (originep o)

    a <- treeViewGetSelection difftreeview
    b <- treeSelectionGetSelectedRows a
    on celltg cellToggled $ \(b::[Char]) -> do toggleStuff difftreeview difstore

-- Start the window and initiate the GUI for use
    widgetShowAll window
    mainGUI
--Function giving us a string representation of subjects
--gnu :: [Object] -> String
--gnu m = case Data.List.map (H.lookup "name") m of
--          Just (x:xs) -> gnu xs
--          Nothing  -> ""
--gnu' :: Object -> String
--gnu' (x:xs) = toString.encode x ++ ";" ++ gnu' xs
--gnu' [] = " "

cmfo u o = Methode {nom=titlep o
                   ,domaine=domainp  o
                   ,origine=originep o
                   ,lien=lienp o
                   ,description=descriptionp o
                   ,sourceName=u}
chainAtomIO:: IORef (Map Int Methode) -> [Methode] ->IO()
chainAtomIO mr (x:xs)  = do atomicModifyIORef mr (\m->(addMethToMap x m ,()))
                            chainAtomIO mr xs
chainAtomIO _ []       = return()
--This function takes all urls and all results from SymeBot and create a list
--of methods with it
pleaseKillMe (x:xs) (y:ys) = Prelude.map (cmfo x) y : pleaseKillMe xs ys
pleaseKillMe [] []         = []
snort :: [[Methode]] -> IORef (Map Int Methode) -> IO()
snort (x:xs) mr           = do chainAtomIO mr x
                               snort xs mr
snort [] _                = return()
