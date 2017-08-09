{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import System.Environment
import Data.List
import Data.List.Split
main =  getArgs >>= handleArgs

handleArgs::[String] -> IO()
handleArgs [sourcetype,source] | sourcetype == "file" = scrapeFromFile source
                               | sourcetype == "url"  = scrapeFromURL source
                               | otherwise            = print $ "Entrée: source, type de source"
handleArgs _ = putStrLn "Erreur dans l'entrée"

apiResults :: Scraper String [String]
apiResults = texts "pre"

--TODO: Find a pattern to separate courses
separateCourses = fmap ((endBy "},\n").(!!1))

delSpace = filter (\x -> x/=' ')

extractedContent ::String         --Field to be extracted
                -> Maybe [String] --Output of apiResults 
                -> Maybe [String] --List of titles
extractedContent s = fmap (filter (\x -> s `isInfixOf` x).lines.(!!1))

scrapeFromFile :: String -> IO()
scrapeFromFile fp = do  c <- readFile fp
                        let i = scrapeStringLike c apiResults
                        parseByFlavour fp i

scrapeFromURL :: String -> IO()
scrapeFromURL url = do  i <- scrapeURL url apiResults
                        parseByFlavour url i 

parseByFlavour s i | "fun-mooc" `isInfixOf` s = funmoocFlavour i
                   | otherwise = print "no flavour configured for this input"

funmoocFlavour i = do let results = fmap (map delSpace) i
                      let titles= extractedContent "title" results
                      let courses = separateCourses results
                      --print $  results
                      print $ fmap (!!1) courses
                      --print $ fmap (!!54) titles
