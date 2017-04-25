{--
Data management helper functions. Fetch Methodinno type values from various sources, 
be it files, string, or direct values.
--}
--module MethManage (setNewId
--                  , fetchFromFile
--                  , delFromFile
--                  ) where
import Data.List
import qualified Data.Map as Map
import System.IO
import qualified System.IO.Strict as SIO
import System.Environment
import System.Directory
import Control.DeepSeq
import Control.Exception
import Methodinno

getIdLst :: [String] -> [String]
getIdLst [] = []
getIdLst (x:xs) = [head x]:getIdLst xs

getValLst :: [String] -> [String]
getValLst [] = []
getValLst (x:xs) = pop x:getValLst xs

pop :: [a] -> [a]
pop [] = []
pop (x:xs) = xs

createMethFromLst :: [String] -> [Maybe Methode]
createMethFromLst [] = []
createMethFromLst (x:xs) = createMethFromVal x : createMethFromLst xs

createMethFromVal :: String -> Maybe Methode
createMethFromVal [] = Nothing
createMethFromVal l = Just Methode {nom=x!!0,domaine=x!!1,origine=x!!2
                               ,pos=read(x!!3)::Position,description=x!!4,lien=x!!5}
                        where x = wordsWhen (==';') l

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'                                
getMapMeth = do
        contents <- readFile "onche"
        let lc = lines contents
        let idLst = getIdLst lc
        let vaLst = getValLst lc
        let methLst = createMethFromLst vaLst
        let lm =  zipWith (\key values -> (key,values)) idLst methLst
        let mapMeth = Map.fromList lm
        print mapMeth




--delFromFile :: [String] -> IO()
--delFromFile [nomFichier, indice] = do
--    contents <- readFile nomFichier
--    let fichierLu  = lines contents
--        fichierLuNum = zipWith (\n line -> show n ++ " - " ++ line) [0..] fichierLu
--    putStrLn "Voici le fichier: "
--    mapM_ putStrLn fichierLuNum
--    let indiceInt = read indice
--        nouveauFichier = unlines $ delete (fichierLu !! indiceInt) fichierLu
--    bracketOnError (openTempFile "." "temp")
--        (\(tempName,tempHandle) -> do
--            hClose tempHandle
--            removeFile tempName)
--        (\(tempName,tempHandle) -> do
--            hPutStr tempHandle nouveauFichier
--            hClose tempHandle
--            removeFile nomFichier
--            renameFile tempName nomFichier)

