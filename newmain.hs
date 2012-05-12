import Control.Monad
import Text.Regex.Posix
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import Debug.Trace
import Text.Printf(printf)
import Text.Regex.Posix
import Data.List
import Data.Maybe
import Char
import Locale
--import Prelude hiding (catch)
import qualified Control.Exception as C


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
} deriving (Show,Eq)

data Track = Track {
  track_id :: Int,
  track_name :: String,
  track_starts :: [Time], -- lista dokładnych momentów (data+czas) wyruszenia pociągu z pierwszej stacji
  track_stations :: [TrackStation]
} deriving (Show) --trzeba zakomentować

data Tracks = Tracks {
  track_counter :: Int,
  tracks :: [Track]
} deriving (Show)

instance Show Station where
  show stn = printf "id: %d nazwa: %s" (station_id stn) (station_name stn)
  
--instance Show Track where
--  show tr = printf "id: %d nazwa: %s" (track_id tr) (track_name tr)

addSecondsToUTCTime secs time = posixSecondsToUTCTime ((utcTimeToPOSIXSeconds time) + (realToFrac secs :: POSIXTime) )

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
      putStrLn "Edycja kursów"
      trs' <- tracksEdit stns trs
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
    "0" -> return stns
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
              
--dodaje nową stację           
addStation stns = do
  putStr "podaj nazwę stacji: "
  stn <- getLine
  let stns' = Stations {station_counter = (station_counter stns + 1), 
  stations = (stations stns) ++ [Station {station_name = stn, station_id = (station_counter stns + 1)}]}
  return stns'

--zmienia nazwę stacji  
editStation stns = do
  mapM_  (putStrLn . show) (stations stns)
  return stns
  putStr "podaj id stacji, którą chcesz edytować: "
  stnid <- getLine
  if stnid == [] || checkDigits stnid == False
  then do
    putStrLn notFounIdxError
    return stns
  else do
    let numid =  read stnid :: Int
    case (find (\v -> (station_id v == numid)) (stations stns)) of
      Nothing -> do 
        putStrLn notFounIdxError
        return stns
      Just found -> do 
        putStr "podaj nową nazwę stacji: "
        newname <- getLine
        let stns' = Stations { stations = ((filter (\x -> ((station_id x) /= numid)) (stations stns)) ++ [Station {station_name = newname, station_id = (station_id found) }]), station_counter = (station_counter stns)}
        putStrLn "jest"
        return stns'
  where
  notFounIdxError = "Nie ma takiego indeksu"

--usuwa stację
deleteStation stns = do
  mapM_  (putStrLn . show) (stations stns)
  return stns
  putStr "podaj id stacji, którą chcesz usunąć: "
  stnid <- getLine
  if stnid == [] || checkDigits stnid == False
  then do
    putStrLn notFounIdxError
    return stns
  else do
    let numid =  read stnid :: Int
    case (find (\v -> (station_id v == numid)) (stations stns)) of
      Nothing -> do 
        putStrLn notFounIdxError
        return (stns)
      Just found -> do 
        let stns' = Stations { stations = (filter (\x -> ((station_id x) /= numid)) (stations stns)), station_counter = (station_counter stns)}
  --      let stns' = Stations { stations = (filter (\x -> ((station_id x) /= numid)) (stations stns)), station_counter = (station_counter stns)-1}
        putStrLn "usunięte"
        return stns'
  where
  notFounIdxError = "Nie ma takiego indeksu"

---------------------------------------
-- 2 Edycja kursów            
---------------------------------------
tracksEdit stns trs = do
  --return trs
  putStrLn tracksHelp
  opt <- getLine
  case opt of
    "1" -> do
      putStrLn "Lista kursów:"
      mapM_  (putStrLn . show) (tracks trs)
      tracksEdit stns trs
    "2" -> do
      trs' <- addTrack stns trs
      putStrLn $ show trs'
      tracksEdit stns trs'
    "3" -> do
      trs' <- editTrack stns trs
      tracksEdit stns trs'
    "4" -> do 
      trs' <- deleteTrack trs
      tracksEdit stns trs'
    "0" -> return(trs)
    _ -> do
      putStrLn "Nieznana komenda\n"
      tracksEdit stns trs
  where
  tracksHelp = "naciśnij:\n\
              \1 - aby wypisać kursy\n\  
              \2 - aby dodać kurs\n\
              \3 - aby edytować kurs\n\
              \4 - aby usunąć kurs\n\
              \0 - aby powrocić do menu głównego"

--dodanie nowego kursu              
addTrack stns trs = do
  putStrLn "podaj nazwę kursu: "
  new_name <- getLine
  print new_name
  --pętla wybierania kolejnych stacji
  let new_stations = []
  new_stations <- addStationsLoop stns new_stations
  if new_stations == []
  then return trs 
  else do
    --pętla wpisywania dat występowania kursu
    let new_starts = []
    new_starts <- addDate new_starts
    if new_starts == [] 
    then return trs
    else do
      let trs' = Tracks {track_counter = (track_counter trs + 1), 
      tracks = (tracks trs) ++ [Track {track_name = new_name, track_id = (track_counter trs + 1), track_starts = new_starts, track_stations = new_stations}]}
      return (trs')  

--tworzenie listy stacji dla kursu
addStationsLoop stns new_stations = do
--dodać zapytanie o kontynuację
  putStrLn "Lista stacji:"
  mapM_  (putStrLn . show) (stations stns)
  putStrLn "podaj id stacji: "
  stnid <- getLine
  if stnid == [] || checkDigits stnid == False
  then do
    putStrLn notFounIdxError
    return []
    --return new_stations
  else if new_stations /= [] && (read stnid :: Int) == stn_id (last new_stations)
  then do
    putStrLn "Dwie kolejne stacje muszą być inne!"
    return []
    --addStationsLoop stns new_stations
  else do
    let numid =  read stnid :: Int
    case (find (\v -> (station_id v == numid)) (stations stns)) of
      Nothing -> do 
        putStrLn notFounIdxError
        return new_stations
      Just found -> do             
        putStrLn "podaj czas postoju w minutach: "
        new_stop <- getLine
        if checkDigits new_stop && new_stop /= []
        then do
          let new_stop' =  read new_stop :: Int
          putStrLn "podaj czas dojazdu do kolejnej stacji w minutach (0 - oznacza koniec kursu): "
          new_travel <- getLine
          if checkDigits new_travel && new_travel /= []
          then do
            let new_travel' =  read new_travel :: Int
            let new_stations' = new_stations ++ [TrackStation {stn_id = numid, stop_time = new_stop', travel_time = new_travel'}]
            if new_travel == "0"
            then 
              return new_stations'
            else do
              addStationsLoop stns new_stations'
          else do
            putStrLn badTimexError
            return []
        else do
          putStrLn badTimexError
          return []
  where
  notFounIdxError = "Nie ma takiego indeksu" 
  badTimexError = "Błędnie podany czas"

--pozwala modyfikować wskazany kurs
editTrack stns trs = do
  putStrLn editTrackHelp
  opt <- getLine
  case opt of
    "1" -> do
      trs' <- reNameTrack trs
      editTrack stns trs'
    "2" -> do
      trs' <- reDateTrack trs
      editTrack stns trs'
    "3" -> do
      trs' <- reStationsTrack stns trs
      editTrack stns trs'
    "0" -> return trs
    _ -> do
      putStrLn "Nieznana komenda\n"
      editTrack stns trs
  where
  editTrackHelp = "naciśnij:\n\
              \1 - aby zmienić nazwę kursu\n\  
              \2 - aby ponownie wprowadzić daty kursu\n\
              \3 - aby ponownie wprowadzić stacje kursu\n\
              \0 - aby powrócić do menu kursów"

--zmiana nazwy
reNameTrack trs = do
      putStrLn "Lista kursów:"
      mapM_  (putStrLn . show) (tracks trs)
      putStrLn "Wybierz kurs:"
      stnid <- getLine
      if stnid == [] || checkDigits stnid == False
      then do
        putStrLn notFounIdxError
        return trs
      else do
        let numid =  read stnid :: Int
        case (find (\v -> (track_id v == numid)) (tracks trs)) of
          Nothing -> do 
            putStrLn notFounIdxError
            return trs
          Just found -> do 
            putStr "podaj nową nazwę kursu: "
            newname <- getLine
            let trs' = Tracks { tracks = ((filter (\x -> ((track_id x) /= numid)) (tracks trs)) ++ [Track {track_name = newname, track_id = (track_id found), track_starts = (track_starts found), track_stations = (track_stations found) }]), track_counter = (track_counter trs)}
            return trs'
  where
  notFounIdxError = "Nie ma takiego indeksu"     
      
--ponowne wprowadzenie dat
reDateTrack trs = do
      putStrLn "Lista kursów:"
      mapM_  (putStrLn . show) (tracks trs)
      putStrLn "Wybierz kurs:"
      stnid <- getLine
      if stnid == [] || checkDigits stnid == False
      then do
        putStrLn notFounIdxError
        return trs
      else do
        let numid =  read stnid :: Int
        case (find (\v -> (track_id v == numid)) (tracks trs)) of
          Nothing -> do 
            putStrLn notFounIdxError
            return trs
          Just found -> do 
            putStr "wprowadź nowe daty: "
            dates <- addDate []
            if dates == [] 
            then do
              putStrLn "nie wprowadzono zmian"
              return trs 
            else do
              let trs' = Tracks { tracks = ((filter (\x -> ((track_id x) /= numid)) (tracks trs)) ++ [Track {track_name = (track_name found), track_id = (track_id found), track_starts = dates, track_stations = (track_stations found) }]), track_counter = (track_counter trs)}
              return trs'
  where
  notFounIdxError = "Nie ma takiego indeksu"   

--ponowne wprowadzenie stacji
reStationsTrack stns trs = do
      putStrLn "Lista kursów:"
      mapM_  (putStrLn . show) (tracks trs)
      putStrLn "Wybierz kurs:"
      stnid <- getLine
      if stnid == [] || checkDigits stnid == False
      then do
        putStrLn notFounIdxError
        return trs
      else do
        let numid =  read stnid :: Int
        case (find (\v -> (track_id v == numid)) (tracks trs)) of
          Nothing -> do 
            putStrLn notFounIdxError
            return trs
          Just found -> do 
            putStr "wprowadź stacje na nowo: "
            new_stations <- addStationsLoop stns []
            if new_stations == [] 
            then do
              putStrLn "nie wprowadzono zmian"
              return trs 
            else do
              let trs' = Tracks { tracks = ((filter (\x -> ((track_id x) /= numid)) (tracks trs)) ++ [Track {track_name = (track_name found), track_id = (track_id found), track_starts = (track_starts found), track_stations = new_stations }]), track_counter = (track_counter trs)}
              return trs'
  where
  notFounIdxError = "Nie ma takiego indeksu"   

--usuwa wskazany kurs
deleteTrack trs = do
  putStrLn "Lista kursów:"
  mapM_  (putStrLn . show) (tracks trs)
  putStrLn "Wprowadź id kursu, który ma zostać usunięty:"
  trnid <- getLine
  if trnid == [] 
  then do
    putStrLn notFounIdxError
    return trs
  else do
    let numid =  read trnid :: Int
    case (find (\v -> (track_id v == numid)) (tracks trs)) of
      Nothing -> do 
        putStrLn notFounIdxError
        return trs
      Just found -> do 
        let trs' = Tracks { tracks = (filter (\x -> ((track_id x) /= numid)) (tracks trs)), track_counter = (track_counter trs)}
        putStrLn "kurs został usunięty"
        return trs'
  where
  notFounIdxError = "Nie ma takiego indeksu"

--sprawdza czy znaki zawierają wyłącznie cyfry
checkDigits [] = True
checkDigits (x:xs) = if isDigit x then
			checkDigits (xs) else
			False    
      
--wczytuje datę wraz z godziną i minutami      
addDate dates =  do
  putStrLn "Podaj datę w formacie: rrrr-mm-dd hh:mm"
  x <- getLine
  let Just x1 = parseTime defaultTimeLocale "%F %R" x :: Maybe UTCTime;
  res <- C.try(print x1)::IO (Either C.SomeException ())
  case res of
    Left err -> do putStrLn "Błędnie podana data!"
                   addDate dates
    Right q  -> do
      putStrLn addDateHelp
      sel <- getLine
      case sel of
        "1" -> do addDate (dates ++ [x1])
        "0" -> return (dates ++ [x1])
        _ -> do
          putStrLn "Nieznana komenda\n"
          addDate dates
  where
  addDateHelp = "naciśnij:\n\
                \1 - aby wprowadzić kolejną datę\n\  
                \0 - aby zakończyć wpisywanie dat"                 
---------------------------------------
-- 3 Wyznaczenie trasy             
---------------------------------------
--getRoute

---------------------------------------
-- 4 Wygenerowanie rozkładu jazdy            
---------------------------------------
--generateSchedule stns trs 

