import Control.Monad
import Text.Regex.Posix
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Debug.Trace
import Text.Printf(printf)
import Text.Regex.Posix
import Data.List


type StationId = Int
type Time = UTCTime
-- w minutach
type Duration = Int

data WeekDay = Mon|Tue|Wed|Thu|Fri|Sat|Sun deriving (Enum, Show)

data Station = Station {
  station_id :: StationId,
  station_name :: String
} deriving (Eq)

data Stations = Stations {
  station_counter :: Int,
  stations :: [Station]
} deriving (Show)

data TrackStation = TrackStation {
  stn_id :: StationId,
  stop_time :: Duration, -- czas postoju na stacji
  travel_time :: Duration -- czas podróży do następnej stacji
} deriving (Show)

data Track = Track {
  track_id :: Int,
  track_name :: String,
  track_starts :: [Time] -- lista dokładnych momentów (data+czas) wyruszenia pociągu z pierwszej stacji
  track_stations :: [TrackStation]
} deriving (Show)

data Tracks = Tracks {
  track_counter :: Int,
  tracks :: [Track]
} deriving (Show)

instance Show Station where
  show stn = printf "id: %d nazwa: %s" (station_id stn) (station_name stn)

main = do
  putStrLn "Witaj w programie Timetable"
  let stns = (Stations {station_counter = 0, stations = []})
  let trs = (Tracks {track_counter = 0, tracks = []})
  mainMenu stns trs
  return ()


mainMenu stns trs = do
  putStrLn mainHelp
  opt <- getLine
  case opt of
    "1" -> do
      putStrLn "Edycja stacji"
      stns' <- stationsEdit stns
      mainMenu stns' trs
    "2" -> do
      putStrLn "Edycja kursów - BRAK"
      trs' <- tracksEdit trs
      mainMenu stns trs'
    "3" -> do
      putStrLn "Wyznaczenie trasy - BRAK"
      mainMenu stns trs
    "4" -> do
      putStrLn "Wygenerowanie rozkładu jazdy - BRAK"
      mainMenu stns trs
    "0" -> do
      putStrLn "Wyjście z programu"
      return()
    _ -> do
      putStrLn "Nieznana komenda\n"
      mainMenu stns trs
  return ()
  where
  mainHelp = "naciśnij:\n\
              \1 - aby edytować stacje\n\
              \2 - aby edytować kursy\n\
              \3 - aby wyznaczyć trasę\n\
              \4 - aby wygenerować rozkład jazdy\n\
              \0 - aby wyjść z programu"

---------------------------------------
-- 1 Edycja stacji              
---------------------------------------
stationsEdit stns = do
  putStrLn stationsHelp
  opt <- getLine
  case opt of
    "1" -> do
      putStrLn "Lista stacji:"
      mapM_  (putStrLn . show) (stations stns)
      stationsEdit stns
    "2" -> do
      stns' <- addStation stns
      --trace show stns'
      putStrLn $ show stns'
      stationsEdit stns'
    "3" -> do
      stns' <- editStation stns
      stationsEdit stns'
    "4" -> do 
      stns' <- deleteStation stns
      stationsEdit stns' -- uwzględnić istniejące kursy!!
    "0" -> return(stns)
    _ -> do
      putStrLn "Nieznana komenda\n"
      stationsEdit stns
  where
  stationsHelp = "naciśnij:\n\
              \1 - aby wypisać stacje\n\  
              \2 - aby dodać stację\n\
              \3 - aby edytować stację\n\
              \4 - aby usunąć stację\n\ 
              \0 - aby powrocić do menu głównego"
              
              
addStation stns = do
  putStr "podaj nazwę stacji: "
  stn <- getLine
  let stns' = Stations {station_counter = (station_counter stns + 1), 
  stations = (stations stns) ++ [Station {station_name = stn, station_id = (station_counter stns + 1)}]}
  return (stns')

editStation stns = do
  mapM_  (putStrLn . show) (stations stns)
  return (stns)
  putStr "podaj id stacji, którą chcesz edytować: "
  stnid <- getLine
  let numid =  read stnid :: Int
  case (find (\v -> (station_id v == numid)) (stations stns)) of
    Nothing -> do 
      putStrLn notFounIdxError
      return (stns)
    Just found -> do 
      putStr "podaj nową nazwę stacji: "
      newname <- getLine
      let stns' = Stations { stations = ((filter (\x -> ((station_id x) /= numid)) (stations stns)) ++ [Station {station_name = newname, station_id = (station_id found) }]), station_counter = (station_counter stns)}
      putStrLn "jest"
      return (stns')
  where
  notFounIdxError = "Nie ma takiego indeksu"

deleteStation stns = do
  mapM_  (putStrLn . show) (stations stns)
  return (stns)
  putStr "podaj id stacji, którą chcesz usunąć: "
  stnid <- getLine
  let numid =  read stnid :: Int
  case (find (\v -> (station_id v == numid)) (stations stns)) of
    Nothing -> do 
      putStrLn notFounIdxError
      return (stns)
    Just found -> do 
      let stns' = Stations { stations = (filter (\x -> ((station_id x) /= numid)) (stations stns)), station_counter = (station_counter stns)-1}
      putStrLn "usunięte"
      return (stns')
  where
  notFounIdxError = "Nie ma takiego indeksu"

---------------------------------------
-- 2 Edycja kursów            
---------------------------------------
tracksEdit trs = do
  --return trs
  putStrLn tracksHelp
  opt <- getLine
  case opt of
    "1" -> do
      putStrLn "Lista kursów:"
      mapM_  (putStrLn . show) (tracks trs)
      tracksEdit trs
    "2" -> do
      trs' <- addTrack trs
      putStrLn $ show trs'
      tracksEdit trs'
    "3" -> do
      trs' <- editTrack trs
      tracksEdit trs'
    "4" -> do 
      trs' <- deleteTrack trs
      tracksEdit trs'
    "0" -> return(trs)
    _ -> do
      putStrLn "Nieznana komenda\n"
      tracksEdit trs
  where
  tracksHelp = "naciśnij:\n\
              \1 - aby wypisać kursy\n\  
              \2 - aby dodać kurs\n\
              \3 - aby edytować kurs\n\
              \4 - aby usunąć kurs\n\
              \0 - aby powrocić do menu głównego"
  
  
addTrack trs = return trs

editTrack trs = return trs

deleteTrack trs = return trs
---------------------------------------
-- 3 Wyznaczenie trasy             
---------------------------------------
--getRoute

---------------------------------------
-- 4 Wygenerowanie rozkładu jazdy            
---------------------------------------
--generateSchedule stns trs 

