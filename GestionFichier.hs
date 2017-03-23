{--
Ajoute, modifie ou supprime des méthodes dans un fichier spécifié en entrée.
--}
import Data.List
import System.IO
import System.Environment
import System.Directory
import Control.Exception

main = do
    (commande:arguments) <- getArgs
    dispatch commande arguments


dispatch :: String -> [String] -> IO()
dispatch "ajouter"   = ajouter
dispatch "voir"      = voir
dispatch "supprimer" = supprimer

ajouter :: [String] -> IO()
ajouter [nomFichier, texte] = appendFile nomFichier (texte ++ "\n")

voir :: [String] -> IO()
voir [nomFichier] = do 
    contents <- readFile nomFichier
    let fichierLu  = lines contents
        fichierLuNum = zipWith (\n line -> show n ++ " - " ++ line) [0..] fichierLu
    putStr $ unlines fichierLuNum

supprimer :: [String] -> IO()
supprimer [nomFichier, indice] = do
    contents <- readFile nomFichier
    let fichierLu  = lines contents
        fichierLuNum = zipWith (\n line -> show n ++ " - " ++ line) [0..] fichierLu
    putStrLn "Voici le fichier: "
    mapM_ putStrLn fichierLuNum
    let indiceInt = read indice
        nouveauFichier = unlines $ delete (fichierLu !! indiceInt) fichierLu
    bracketOnError (openTempFile "." "temp")
        (\(tempName,tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName,tempHandle) -> do
            hPutStr tempHandle nouveauFichier
            hClose tempHandle
            removeFile nomFichier
            renameFile tempName nomFichier)
