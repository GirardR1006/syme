{-#LANGUAGE ScopedTypeVariables#-}

{--
    GUI handling.
--}

module SymeUI where

import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import Data.Text as T
import Data.Maybe
import Data.Map as M
import MethManage
import Methodinno
hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]
coucou :: (ButtonClass o) => o -> IO ()
coucou b = set b [buttonLabel := "Coucou"]
--toto :: (ComboBoxClass o, ButtonClass b) => o -> i -> IO()
--toto o b = set b [buttonLabel := o]
--titi :: (TextViewClass o, ButtonClass b) => o -> b -> IO()
--titi o b = textViewSetBuffer o (buttonGetLabel b)
assignComboTextToBuffer :: (ComboBoxClass o, TextBufferClass i) => o -> i -> IO()
assignComboTextToBuffer o i = do
            tmp <- comboBoxGetActiveText o
            case tmp of Just str -> textBufferSetText i (T.unpack str)      
                        Nothing  -> textBufferSetText i ""
registerMeth :: EntryClass a => a -> a -> a -> a -> a -> a -> IO(Methode)
registerMeth nameE fldE posE orgnE dscpE linkE = do
                                            name <- entryGetText nameE
                                            fld <- entryGetText fldE
                                            tmppos <- entryGetText posE
                                            let position = posCheck tmppos
                                            orgn <- entryGetText orgnE
                                            dscp <- entryGetText dscpE
                                            link <- entryGetText linkE
                                            putStrLn ("name: " ++ name)
                                            putStrLn ("field: " ++ fld)
                                            putStrLn ("position: " ++ position)
                                            putStrLn ("origin: " ++ orgn)
                                            putStrLn ("link: " ++ link)
                                            putStrLn ("description: " ++ dscp)
                                            return( Methode {nom=name
                                                             ,domaine=fld
                                                             ,origine=orgn
                                                             ,pos=read(position)::Position
                                                             ,description=dscp
                                                             ,lien=link} )
                                              where posCheck s 
                                                        | s == "" = show(Position (0,0))
                                                        | s!!0 /= '(' = show(Position (0,0))
                                                        | otherwise = s
currentTvwSelection :: TreeView -> ListStore (String,String) -> IO() 
currentTvwSelection t m = do 
                     --tvwM <- treeViewGetModel t 
                     tvwS <- treeViewGetSelection t
                     tvwP <- treeSelectionGetSelectedRows tvwS
                     let s = Prelude.head (Prelude.head tvwP)
                     v <- listStoreGetValue m s
                     putStrLn $ "selected" ++ (snd v)

symeui :: IO ()
symeui = do
    initGUI
-- Loading the static interface definition from external file
    builder <- builderNew
    builderAddFromFile builder "./src/SymeUI.glade"
    window   <- builderGetObject builder castToWindow "mainWindow"
    dialogadd <- builderGetObject builder castToDialog "addMethDialog"
    window `on` deleteEvent $ liftIO mainQuit >> return False
    dialogadd `on` deleteEvent $ liftIO (widgetHide dialogadd) >> return False
    --Button definition
    abttn <- builderGetObject builder castToButton "addBttn"
    sbttn <- builderGetObject builder castToButton "showBttn"
    qbttn <- builderGetObject builder castToButton "quitBttn"
    abttnadd <- builderGetObject builder castToButton "addMethaddBttn"
    qbttnadd <- builderGetObject builder castToButton "addMethquitBttn"
    --Comboboxes definition
    scbbx <- builderGetObject builder castToComboBox "searchCbbx"
    comboBoxSetModelText scbbx
    comboBoxAppendText scbbx $ T.pack "Nom" 
    comboBoxAppendText scbbx $ T.pack "Domaine d'application" 
    comboBoxAppendText scbbx $ T.pack "Origine" 
    comboBoxAppendText scbbx $ T.pack "Description" 
    --Textdisplay definition
    nametxt <- builderGetObject builder castToTextView "nameTxt"
    domtxt <- builderGetObject builder castToTextView "fldTxt"
    --Textbuffers definition
    txtbuffer <- textBufferNew Nothing
    txtbuffer2 <- textBufferNew Nothing
    --Text entries definition
    nametxtadd <- builderGetObject builder castToEntry "addMethnameTxt"
    fldtxtadd <- builderGetObject builder castToEntry "addMethfldTxt"
    postxtadd <- builderGetObject builder castToEntry "addMethposTxt"
    orgntxtadd <- builderGetObject builder castToEntry "addMethorgnTxt"
    dscptxtadd <- builderGetObject builder castToEntry "addMethdscpTxt"
    linktxtadd <- builderGetObject builder castToEntry "addMethlinkTxt"
    --Loading data from file 
    initialmap <- getMapMeth "src/onche"
    
    --TreeView initialization and configuration
    restreeview <- builderGetObject builder castToTreeView "resultTreeView"
    rescolumn <- builderGetObject builder castToTreeViewColumn "resultTreeViewColumn"
    idcolumn <- builderGetObject builder castToTreeViewColumn "resultIDTreeViewColumn"
    celltxt <- builderGetObject builder castToCellRendererText "resultTreeViewCell"
    cellidtxt <- builderGetObject builder castToCellRendererText "resultIDTreeViewCell"
    let listofNames = Prelude.map (nom.snd) (M.toList initialmap)
    let listofIDs   = Prelude.map fst (M.toList initialmap)
    let joinedlist = Prelude.zipWith (\a b -> (show(a),b)) listofIDs listofNames
    store <- listStoreNew joinedlist
    cellLayoutSetAttributes rescolumn celltxt store $ \x -> [cellText :=  snd x]
    cellLayoutSetAttributes idcolumn cellidtxt store $ \x -> [cellText :=  fst x]
    treeViewSetModel restreeview store

-- Linking functions to signals
-- Note that the only relevant signals here are those defined
-- in the Gtk package documentation on Hackage. 
-- Handlers defined in Glade are of no use here.

--Main window signals
--Buttons
    abttn `on` buttonActivated $ do
                                   -- assignComboTextToBuffer scbbx txtbuffer2
                                   -- textViewSetBuffer domtxt txtbuffer2
                                   -- widgetShowAll windowadd
                                   onche <- dialogRun dialogadd
                                   --TODO: find a more efficient way to handle
                                   --dialog than simply hide it
                                   if onche == ResponseOk 
                                       then putStrLn "Ok signal received, adding method."
                                       >> widgetHide dialogadd
                                   else
                                       putStrLn "Cancel method received, hiding window."
                                       >> widgetHide dialogadd

    sbttn `on` buttonActivated $ do
                                   currentTvwSelection restreeview store
    qbttn `on` buttonActivated $ do
                                    putStrLn "Saving data in storage..."
                                    saveMapInFile initialmap "src/onche"
                                    putStrLn "Quitting project Syme..."
                                    mainQuit
--TreeView signals
--Add method window signals
    abttnadd `on` buttonActivated $ do 
                                    newmeth <- registerMeth nametxtadd fldtxtadd postxtadd orgntxtadd linktxtadd dscptxtadd
                                    putStrLn (show newmeth)
                                    listStoreAppend store (show((maxKeyInMap initialmap) + 1),(nom newmeth))
                                    --newMap <- addMethToMap newmeth initialmap  
                                    dialogResponse dialogadd ResponseOk 
    qbttnadd `on` buttonActivated $ dialogResponse dialogadd ResponseCancel

-- Start the window and initiate the GUI for use
    widgetShowAll window
    mainGUI
