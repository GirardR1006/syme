--Manage the current and potential web-scraping targets, R/W actions and complicated date stuff
--Source information will be contained in an outside file, and related methods
--will be loaded afterwards. So createSource will only be called when we already
--have a M.Map Int Methode in memory.

import Data.Map as M
import Data.Time
import Data.List.Split
import MethManage

data Source = Source {sourceName::String
                     ,lastVisit::Day
                     ,relatedMethods::M.Map Int Methode}

sourcesFromFile :: String             --Date and name read from file
                -> IO([(String,Day)]) --Part of data used to 
sourcesFromFile fp = do lc <- fmap lines $ readFile fp
                        let nameList      = Prelude.map (head.splitOn ";") lc
                        let lastVisitList = Prelude.map (head.tail.splitOn ";") lc
                        let nameAndDate =  zipWith (\key values -> (read(key) :: Int,values)) idLst methLst
                        return nameAndDate

createSource :: (String,Day)
             -> M.Map Int Methode         
             -> Source  
createSource (n,d) m = Source {sourceName=n,lastVisit=d,relatedMethods=m}
createSource _ _ = Source {source="",lastVisit=ModifiedJulianDay 0,relatedMethods=M.empty}

saveSourcesInFile :: [Sources]
                  -> String   --Filepath
                  -> IO()
saveSourcesInFile s fp = do let names  = Prelude.map sourceName s
                            let lastVisits = Prelude.map (show.lastVisit) s
                            let toBeWritten = zipWith (\x y -> x++";"++y++";") name lastVisits
                            copyFile fp (fp ++ ".txt.tmp")
                            writeFile fp ""
                            mapM_ (appendFile fp) $ toBeWritten 

curDate = do date <- fmap (head.(splitOn " ").show) $ getCurrentDate
             let datelist = splitOn "-" date
             let year = datelist!!0
             let month = datelist!!1
             let day = datelist!!2
             return fromGregorian (read(year)::Integer) (read(month)::Int) (read(day)::Int)
