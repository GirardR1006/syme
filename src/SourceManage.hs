{--
Manage the current and potential web-scraping targets, R/W actions and complicated date stuff
Source information will be contained in an outside file, and related methods
will be loaded afterwards. So createSource will only be called when we already
have a M.Map Int Methode in memory.
--}
module SourceManage
(sourcesFromFile
,createSource
,saveSourcesInFile
,curDate
) where
import System.Directory
import Data.Map as M
import Data.Time
import Data.List.Split
import Methodinno
import MethManage

data Source = Source {sourceid::String
                     ,lastVisit::Day
                     ,relatedMethods::M.Map Int Methode} 
                     deriving (Show)

sourcesFromFile :: String             --Date and name read from file
                -> IO([(String,Day)]) --Part of data used to 
sourcesFromFile fp = do lc <- fmap lines $ readFile fp
                        let names      = Prelude.map (head.splitOn ";") lc
                        let lastVisits = Prelude.map (head.tail.splitOn ";") lc
                        let namesAndDates =  zipWith (\n lv -> (n,read(lv)::Day)) names lastVisits
                        return namesAndDates
--Create a Source value with relatedMethods field containing all Methode values
--with sourceName equal to Source sourceid field
createSource :: (String,Day)
             -> M.Map Int Methode         
             -> Source  
createSource (n,d) m = case searchBy "Source" n m of
                           Nothing->Source {sourceid=n,lastVisit=d,relatedMethods=M.empty}
                           Just x-> Source {sourceid=n,lastVisit=d,relatedMethods=x}
createSource _ _ = Source {sourceid="",lastVisit=ModifiedJulianDay 0,relatedMethods=M.empty}

saveSourcesInFile :: [Source]
                  -> String   --Filepath
                  -> IO()
saveSourcesInFile s fp = do let names  = Prelude.map sourceid s
                            let lastVisits = Prelude.map (show.lastVisit) s
                            let toBeWritten = zipWith (\x y -> x++";"++y++";") names lastVisits
                            copyFile fp (fp ++ ".txt.tmp")
                            writeFile fp ""
                            mapM_ (appendFile fp) $ toBeWritten 

curDate = do date <- fmap (head.(splitOn " ").show) $ getCurrentTime
             let datelist = splitOn "-" date
             let year = datelist!!0
             let month = datelist!!1
             let day = datelist!!2
             return $ fromGregorian (read(year)::Integer) (read(month)::Int) (read(day)::Int)
