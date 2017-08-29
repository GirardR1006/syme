{-# LANGUAGE OverloadedStrings #-}

module Symebot(
 RawO(..)
,defO 
,symeScrape
)
where
import Text.HTML.Scalpel
import System.Environment
import Data.List
import Data.List.Split
import Data.Time
import Data.Maybe
data RawO = RawO {titlep::String
                 ,univp::String
                 ,domp::[String]
                 ,fkeyp::String}
                 deriving (Show)
defO = RawO {titlep="",univp="",domp=[""],fkeyp=""}

symeScrape [sourcetype,source] | sourcetype == "file" = scrapeFromFile source
                               | sourcetype == "url"  = scrapeFromURL source
                               | otherwise            = print "succ"  >> return []
symeScrape _ = print "Usage: handleArgs [file|url] fichier|url" >> return []

scrapeFromFile fp = do  c <- readFile fp
                        let i = scrapeStringLike c apiResults
                        parseByFlavour fp i

scrapeFromURL url = do  i <- scrapeURL url apiResults
                        print i
                        parseByFlavour url i 

--Ajouter ici les différentes stratégies de parsing
parseByFlavour s i | "fun-mooc" `isInfixOf` s = do funmoocFlavour i
                   | otherwise = print "no flavour configured for this input" >> return []

--Stratégie pour le site fun-mooc. Cherche titre du cours, université, domaine
--et clef étrangère et les renvoie sous forme de liste.
funmoocFlavour i = do let courses = separateCourses i
                      let lignedcourses = fmap (map lines) courses
                      let titles = map (extractedContent "title") <$> courses
                      let univs = map (extractedContent "university_name") <$> courses
                      let fkeys = map (extractedContent "id") <$> courses
                      let domains =  fmap (map listOfDomains) lignedcourses
                      let fmOutput = rawData (fromMaybe [] titles) (fromMaybe [] univs) (fromMaybe [] domains) (fromMaybe [] fkeys)
                      return fmOutput

rawData (x:xs) (y:ys) (z:zs) (f:fs) = RawO {titlep=(fromMaybe "NOTITLE" x)
                                           ,univp=(fromMaybe "NOUNIV" y)
                                           ,domp=z
                                           ,fkeyp=(fromMaybe "NOKEY" f)}:rawData xs ys zs fs
rawData _ _ _ _                     = []
--Extrait la première occurence d'une balise JSON dans une chaîne donnée
extractedContent ::String           --Intitulé que l'on souhaite extraire
                -> String           --Chaîne dans laquelle chercher le terme
                -> Maybe String     --Chaîne trouvée
extractedContent f c = if l == [] 
                       then Nothing
                       else Just (((filter (\x->x/='\"')).(!!1).(splitOn (f++"\":")).(!!0)) l)
                            where 
                            l = (filter (\x -> f `isInfixOf` x).lines) c
--Extrait la liste des thématiques des MOOC depuis un JSon formatté sous forme
--de string
listOfDomains::[String] -> [String]
listOfDomains l = snd(foldl f (False,[]) l)
                where
                f (in_delim,domain) x | "subjects" `isInfixOf` x = (True,domain)
                                      | "],"      `isInfixOf` x  = (False, domain)
                                      | otherwise                = if in_delim
                                                                   then (True, x:domain)
                                                                   else (False, domain)

apiResults :: Scraper String [String]
apiResults = texts "pre"

--Dans fun-mooc, les chaînes décrivant deux cours différent sont séparées par deux tabulations
--ATTENTION: tester pour éviter l'ambiguïté tab/espace!
--Le premier élément de apiResults est une balise GET, qu'on ignore, d'où le !!1
separateCourses = fmap ((endBy "},\n        {").(!!1))

delSpace = filter (\x -> x/=' ')

