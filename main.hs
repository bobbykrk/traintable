import Control.Monad
import Text.Regex.Posix
import Data.Time.Format
import Data.Time.Clock
import System.Locale

type StationId = Int
type StationName = String
data StationData = Station StationId StationName deriving (Show, Eq)
data StationsData = Stations Int [StationData] deriving (Show)

type PathName = String
type PathId = Int
type StationList = [StationId]
type Time = UTCTime
-- typ definiujący pojedynczy kurs pociągu tzn. konkrent przejazd
-- zawiera trójki składające się z id stacji, czasu przyjazdu i czasu odjazdu ze stacji
-- odjazd musi być póżniej niż przyjazd na stację
data TrackData = Track [(StationId, Time, Time)] deriving (Show)
-- typ definiujący relację (w sensie np. Warszawa-Kraków)
-- kolejne parametry to identyfikator relacji, nazwa relacji, lista wszystkich stacji tej relacji,
-- lista tras związanych z tą relacją
-- list tras musi być podciągiem listy stacji
data PathData = Path PathName StationList [TrackData] deriving (Show)

-- dodaje nową stację 
addStation :: StationName -> StationsData -> StationsData
addStation (station) (Stations numer stationList) = 
  Stations (numer+1) (stationList ++ [Station numer station])
-- dodaje już istniejącą stację
addStat :: StationData -> StationsData -> StationsData
addStat station (Stations nr stationList) = Stations nr ([station] ++ stationList)

-- usuwa stację
removeStation :: StationName -> StationsData -> StationsData
removeStation _ (Stations numer []) = Stations numer []
removeStation stationName (Stations numer ((Station id station):rest))
  | station == stationName = Stations (numer - 1) rest
  | otherwise = addStat (Station id station) (removeStation stationName (Stations numer rest))

-- zmienia nazwę stacji
modifyStation :: StationName -> StationName -> StationsData -> StationsData
modifyStation _ _ (Stations numer []) = Stations numer []
modifyStation stationName newStationName (Stations numer ((Station id station):rest))
  | station == stationName = Stations numer ((Station id newStationName):rest)
  | otherwise = addStat (Station id station) (modifyStation stationName newStationName (Stations numer rest))

-- czy relacja o podanej nazwie już istnieje
isPathExists :: PathName -> [PathData] -> Bool
isPathExists _ [] = False
isPathExists pathName ((Path exPathName _ _):paths)
  | pathName == exPathName = True
  | otherwise = isPathExists pathName paths

-- dodaje nową relację
addPath :: PathData -> [PathData] -> [PathData]
addPath newPath pathList = pathList ++ [newPath]

-- usuwa relację
removePath :: PathName -> [PathData] -> [PathData]
removePath pathName paths = filter (\(Path name _ _) -> name /= pathName ) paths


-- sprawdza czy la jes podsekwencją lb
-- do sprawdzania czy trasa jest podsekwencją relacji
isSubseq :: [StationId] -> [StationId] -> Bool
isSubseq [] _ = True
isSubseq _ [] = False
isSubseq (a:la) (b:lb)
  | a == b = isSubseq la lb
  | otherwise = isSubseq (a:la) lb



main = do
  putStrLn "Witaj w programie timetable"
  mainHelp
  let stations = (Stations 0 [])
  stations <- getStation stations
  stacja <- getLine
  let new_stations = (addStation stacja stations)
  let stations = new_stations
  print stations
  let new_stations = removeStation "asd" stations
  let stations = new_stations
  let new_stations = modifyStation "qwe" "qwa" stations
  let stations = new_stations
  print stations
  print "asd"


getStation stations = do
  stacja <- getLine
  let new_stations = (addStation stacja stations)
  return new_stations

mainLoop = do
  putStrLn "asd"

mainHelp = do sequence (map putStrLn ["1 aby dodać stacje", "2 aby modyfikować stacje"])

getStations = do
  line <- getLine
  if line == "" then return []
                else do rest <- getStations
                        return (line:rest)
