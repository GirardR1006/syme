{-# LANGUAGE OverloadedStrings #-}

module Symebot(
 RawO(..)
 ,test
 ,defO 
 ,symeScrape
)
where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Network.HTTP.Conduit
import System.Environment

newtype RawOList = RawOList {rawOList::[RawO]} deriving(Show)
instance FromJSON RawOList where
  parseJSON (Object o) = RawOList <$> o .: "results"

data RawO = RawO {titlep::String
                 ,univp::String
                 ,fkeyp::Int
                 ,domp::[Object]}
                 deriving (Show)
instance FromJSON RawO where
  parseJSON (Object v) = 
    RawO  <$> v.: "title"
          <*> v.: "university_name"
          <*> v.: "id"
          <*> v.: "subjects"
--Precious test function, to be used within ghci to test parsing strategies
--before implementing them
test = do  
         let jSON = simpleHttp "https://www.fun-mooc.fr/fun/api/courses/?rpp=5000&page1"
         d <- (eitherDecode <$> jSON ) :: IO (Either String RawOList)
         case d of 
           Left err -> putStrLn err
           Right ps -> print (domp ((rawOList ps)!!1))        
--Default value of parsing result, used in various places of the program
defO :: RawO
defO = RawO {titlep="NO_RESULT",univp="",domp=[],fkeyp=(-1)}

symeScrape :: [String] -> IO([RawO])
symeScrape [sourcetype,source] | sourcetype == "file" = scrapeFromFile source
                               | sourcetype == "url"  = scrapeFromURL source
                               | otherwise            = print "dank"  >> return []
symeScrape _ = print "Usage: handleArgs [file|url] fichier|url" >> return []

scrapeFromFile fp = do  let c = B.readFile fp
                        parseByFlavour fp c

scrapeFromURL url = do  let c = simpleHttp url
                        parseByFlavour url c 

--Ajouter ici les différentes stratégies de parsing
parseByFlavour s i | "fun-mooc" `isInfixOf` s = do funmoocFlavour i
                   | otherwise = print "no flavour configured for this input" >> return []

--Stratégie pour le site fun-mooc. Cherche titre du cours, université, domaine
--et clef étrangère et les renvoie sous forme de liste.
funmoocFlavour i = do d <- (eitherDecode <$> i) :: IO (Either String RawOList)
                      case d of 
                        Left err -> return [defO]
                        Right ps -> return (rawOList ps)
