{--
Data management helper functions.
Provide data read and write features, as well as research features.
Mostly a wrapper around the Data.Map library.
--}
module MethManage 
(searchBy
,splitStr
,getMapMeth
,addMethToMap
,saveMapInFile
,maxKeyInMap
) where
import Data.List
import qualified Data.Map as Map
import System.IO
import System.Environment
import System.Directory
import Control.Exception
import Methodinno

--Some helper function to parse methods from file

getIdLst :: [String] --Lines from file
         -> [String] --List of IDs
getIdLst [] = []
getIdLst (x:xs) = [head x]:getIdLst xs

getValLst :: [String] --Lines from file
          -> [String] --List of record values 
getValLst [] = []
getValLst (x:xs) = pop x:getValLst xs
                where
                    pop [] = []
                    pop (x:xs) = xs

createMethFromVal :: String  --Record values (Strings separated with semicolons)
                  -> Methode --Methodes value
createMethFromVal [] = Methode {nom="",domaine="",origine=""
                               , pos=read("(0,0)")::Position,description="",lien=""}
createMethFromVal l = Methode {nom=x!!0,domaine=x!!1,origine=x!!2
                              , pos=read(x!!3)::Position,description=x!!4,lien=x!!5}
                        where x = splitStr (==';') l

--Generalized Prelude.words function, split a string according to a boolean
--predicate 
splitStr     :: (Char -> Bool) -> String -> [String]
splitStr p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitStr p s''
                            where (w, s'') = break p s'

--Parse a file into a map of Methodes
getMapMeth fp = do
                 lc <- fmap lines $ readFile fp
                 let idLst = getIdLst lc
                 let vaLst = getValLst lc
                 let methLst = Prelude.map createMethFromVal vaLst
                 let lm =  zipWith (\key values -> (read(key) :: Int,values)) idLst methLst
                 let mapMeth = Map.fromList lm
                 return mapMeth

--Search a Methode in a map regarding the record values
searchBy :: String --The field in which the predicate is supposed to be
         -> String --The predicate we want to find
         -> Map.Map Int Methode --The map we are looking into
         -> Maybe (Map.Map Int Methode) --The map containing values matching the predicate
searchBy "Nom" s m = Map.differenceWith f m <$> (getBoolMap nom s m) 
searchBy "Domaine d'application" s m = Map.differenceWith f m <$> (getBoolMap domaine s m) 
searchBy "Origine" s m = Map.differenceWith f m <$> (getBoolMap origine s m) 
searchBy "Description" s m = Map.differenceWith f m <$> (getBoolMap description s m) 
searchBy "Lien" s m = Map.differenceWith f m <$> (getBoolMap lien s m) 
searchBy _ _ _ = Nothing
--Get a boolean map returning True if s is contained within values of m, False
--otherwise
getBoolMap c s m = Just(Map.map (isInfixOf s) (Map.map c m))
--Helper function to be used with Map.differenceWith to return only the values
--containing the predicate (values that are True if applied getBoolMap)
f v1 v2 = if v2==True then Just v1 else Nothing

maxKeyInMap :: Map.Map Int v  
            -> Int
maxKeyInMap m = foldr max 0 $  Map.keys m

addMethToMap :: Methode  --Value to be added
             -> Map.Map Int Methode --Map in which the value will be added
             -> Map.Map Int Methode --Result map
addMethToMap meth map = Map.insert ((maxKeyInMap map)+1) meth map 

saveMapInFile m fp = do
                      let keyList  = Prelude.map (show.fst) (Map.toList m)
                      let methList = Prelude.map (stringRepresentation.snd) (Map.toList m)
                      let toBeWritten = zipWith (\id s -> id ++ ";" ++ s) keyList methList
                      copyFile fp (fp ++ ".cod.tmp")
                      writeFile fp ""
                      mapM_ (appendFile fp) $ toBeWritten 
                        where stringRepresentation m =
                                                    f(nom m) ++ ";" ++f(domaine m) ++
                                                    ";" ++ f(origine m) ++ ";" ++
                                                    show (pos m) ++";" ++
                                                    f(description m) ++ ";" ++ f(lien m) ++ ";\n"
                                                        where f m = if m == "" then " " else m
