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
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Text
import Network.HTTP.Conduit
import System.Environment
-- Instance to create a RawO from a parsed value
class WannabeRawO a where 
  crRawO :: a -> RawO
--Sanitizer function
si = Data.List.filter ((&&) <$> (/='\n') <*> (/=';'))
-- raw output type, created by collection of different parsed sources
data RawO = RawO {titlep::String
                 ,domainp::String
                 ,originep::String
                 ,descriptionp::String
                 ,lienp::String} deriving (Show)
defO = RawO {titlep="Default title",domainp="Default domain",originep="Default origin"
            ,descriptionp="Default description",lienp="Default link"}
-- type for parsing fun-mooc API
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
---- type for parsing coursera API
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
---- type for parsing udacity API
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
--Precious test function, to be used within ghci to test parsing strategies
--before implementing them
test = do  
         let jSON = B.readFile "../test/udacity"
         d <- (eitherDecode <$> jSON ) :: IO (Either String UdacityList)
         case d of 
           Left err -> putStrLn err
           Right ps -> print (crRawO ((udacityList ps)!!1) )        

symeScrape :: [String] -> IO([RawO])
symeScrape [sourcetype,source] | sourcetype == "file" = scrapeFromFile source
                               | sourcetype == "url"  = scrapeFromURL source
                               | otherwise            = print "dank"  >> return []
symeScrape _ = print "Usage: handleArgs [file|url] fichier|url" >> return []

scrapeFromFile fp = do  let c = B.readFile fp
                        parseByFlavour fp c

scrapeFromURL url = do  let c = simpleHttp url
                        parseByFlavour url c 
--Add here different parsing strategies
parseByFlavour s i | "fun-mooc" `Data.List.isInfixOf` s = do funmoocFlavour i
                   | "coursera" `Data.List.isInfixOf` s = do courseraFlavour i
                   | "udacity"  `Data.List.isInfixOf` s = do udacityFlavour i
                   | otherwise = print "no flavour configured for this input" >> return []
--TODO: filter all output to remove \n and ';' in descriptions
--Also, why the heck do we have a shift in written fields for coursera stuff? 
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
