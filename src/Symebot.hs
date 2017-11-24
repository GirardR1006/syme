{-# LANGUAGE OverloadedStrings #-}

module Symebot(
 RawO(..)
 ,test
 ,scrapeall
 ,defO 
 ,symeScrape
 ,symeScrape'
)
where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Text
import Network.HTTP.Conduit
import System.Environment
import Control.Exception
-- Instance to create a RawO from a parsed value
class WannabeRawO a where 
  crRawO :: a -> RawO
-- Sanitizer function
si = Data.List.filter ((&&) <$> (/='\n') <*> (/=';'))
-- Raw output type, created by collection of different parsed sources
data RawO = RawO {titlep::String
                 ,domainp::String
                 ,originep::String
                 ,descriptionp::String
                 ,lienp::String} deriving (Show)
defO = RawO {titlep="Default title",domainp="Default domain",originep="Default origin"
            ,descriptionp="Default description",lienp="Default link"}
-- Type for parsing fun-mooc API
newtype FunmoocList = FunmoocList {funmoocList::[Funmooc]} deriving(Show)
instance FromJSON FunmoocList where
  parseJSON (Object o) = FunmoocList <$> o .: "results"
data Funmooc = Funmooc {titlefnmc::String
                       ,univfnmc::String
                       ,domfnmc::[Object]} deriving (Show)
instance FromJSON Funmooc where
  parseJSON (Object v) = 
    Funmooc  <$> v.: "title"
             <*> v.: "university_name"
             <*> v.: "subjects"
instance WannabeRawO Funmooc where
  crRawO f = RawO {titlep=(titlefnmc f)
                  ,domainp=show(domfnmc f)
                  ,originep=(univfnmc f)
                  ,descriptionp=""
                  ,lienp=""} 
--Type for parsing coursera API
newtype CourseraList = CourseraList {courseraList::[Coursera]} deriving(Show)
instance FromJSON CourseraList where
  parseJSON (Object o) = CourseraList <$> o .: "elements"
data Coursera = Coursera {titlecrsr::String
                         ,domcrsr::[Object]
                         ,descriptioncrsr::String}
                         deriving (Show)
instance FromJSON Coursera where
  parseJSON (Object v) = 
    Coursera <$> v .: "name"
             <*> v .: "domainTypes"
             <*> v .: "description"
instance WannabeRawO Coursera where
  crRawO f = RawO {titlep=(si.titlecrsr) f
                  ,domainp=si(show(domcrsr f))
                  ,originep=""
                  ,descriptionp=(si.descriptioncrsr) f
                  ,lienp=""} 
--Type for parsing udacity API
newtype UdacityList = UdacityList {udacityList::[Udacity]} deriving(Show)
instance FromJSON UdacityList where
  parseJSON (Object o) = UdacityList <$> o .: "courses"
data Udacity = Udacity {titleudct::String
                       ,subtitleudct::String
                       ,urludct::String
                       ,descriptionudct::String}
                       deriving (Show)
instance FromJSON Udacity where
  parseJSON (Object v) = 
    Udacity <$> v .: "title"
            <*> v .: "subtitle"
            <*> v .: "homepage"
            <*> v .: "summary"
instance WannabeRawO Udacity where
  crRawO f = RawO {titlep=(si((titleudct f) ++ " - " ++ (subtitleudct f)))
                  ,domainp="",originep=""
                  ,descriptionp=(si.descriptionudct) f
                  ,lienp=(si.urludct) f} 
--Type for parsing iversity API
newtype IversityList = IversityList {iversityList::[Iversity]} deriving(Show)
instance FromJSON IversityList where
  parseJSON (Object o) = IversityList <$> o .: "courses"
data Iversity = Iversity {titleict::String
                         ,subtitleict::String
                         ,urlict::String
                         ,descriptionict::String
                         ,domict::String}
                         deriving (Show)
instance FromJSON Iversity where
  parseJSON (Object v) = 
    Iversity <$> v .: "title"
             <*> v .: "subtitle"
             <*> v .: "url"
             <*> v .: "description"
             <*> v .: "discipline"
instance WannabeRawO Iversity where
  crRawO f = RawO {titlep=(si((titleict f) ++ " - " ++ (subtitleict f)))
                  ,domainp=(si (domict f))
                  ,descriptionp=(si.descriptionict) f
                  ,lienp=(si.urlict) f,originep=""} 
--Precious test function, to be used within ghci to test parsing strategies
--before implementing them
test = do  
         let jSON = B.readFile "../test/udacity"
         d <- (eitherDecode <$> jSON ) :: IO (Either String UdacityList)
         case d of 
           Left err -> putStrLn err
           Right ps -> print (crRawO ((udacityList ps)!!1) )     

scrapeall sources = do fmres   <- funmoocFlavour (simpleHttp (sources!!0))
                       crsrres <- courseraFlavour (simpleHttp (sources!!1))
                       udctres <- udacityFlavour (simpleHttp (sources!!2))
                       return([fmres,crsrres,udctres])

symeScrape :: [String] -> IO([RawO])
symeScrape [sourcetype,source] | sourcetype == "file" = scrapeFromFile source
                               | sourcetype == "url"  = scrapeFromURL source
                               | otherwise            = print "dank"  >> return []
symeScrape _ = print "Usage: handleArgs [file|url] fichier|url" >> return []
--Parse a list of urls 
symeScrape' l = mapM scrapeFromURL l


scrapeFromFile fp = do  let c = B.readFile fp
                        parseByFlavour fp c

scrapeFromURL url = do  let c = simpleHttp url
                        parseByFlavour url c
--Add here different parsing strategies
parseByFlavour s i | "fun-mooc" `Data.List.isInfixOf`  s = do funmoocFlavour i
                   | "coursera" `Data.List.isInfixOf`  s = do courseraFlavour i
                   | "udacity"  `Data.List.isInfixOf`  s = do udacityFlavour i
                   | "iversity"  `Data.List.isInfixOf` s = do iversityFlavour i
                   | otherwise = print "no flavour configured for this input" >> return []

funmoocFlavour i = do d <- (eitherDecode <$> i) :: IO (Either String FunmoocList)
                      case d of 
                        Left err -> return [defO]
                        Right ps -> return (Data.List.map crRawO (funmoocList ps))

courseraFlavour i = do d <- (eitherDecode <$> i) :: IO (Either String CourseraList)
                       case d of 
                         Left err -> return [defO]
                         Right ps -> return (Data.List.map crRawO (courseraList ps))

udacityFlavour i = do d <- (eitherDecode <$> i) :: IO (Either String UdacityList)
                      case d of 
                        Left err -> return [defO]
                        Right ps -> return (Data.List.map crRawO (udacityList ps))

iversityFlavour i = do d <- (eitherDecode <$> i) :: IO (Either String IversityList)
                       case d of 
                         Left err -> return [defO]
                         Right ps -> return (Data.List.map crRawO (iversityList ps))
