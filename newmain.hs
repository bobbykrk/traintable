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
  arrival :: Time,
  departure :: Time
} deriving (Show)

data Track = Track {
  track_id :: Int,
  track_name :: Int
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
    "2" -> return()
    "3" -> return()
    "4" -> return()
    "0" -> return()
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

stationsEdit stns = do
  putStrLn stationsHelp
  opt <- getLine
  case opt of
    "1" -> do
      stns' <- addStation stns
      --trace show stns'
      putStrLn $ show stns'
      stationsEdit stns'
    "2" -> do
      stns' <- editStation stns
      stationsEdit stns'
    "3" -> do 
      stns' <- deleteStation stns
      stationsEdit stns'
    "0" -> return(stns)
    _ -> do
      putStrLn "Nieznana komenda\n"
      stationsEdit stns
  where
  stationsHelp = "naciśnij:\n\
              \1 - aby dodać stację\n\
              \2 - aby edytować stację\n\
              \3 - aby usunąć stację\n\
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
      putStr "podaj nową nazwę stacji: "
      newname <- getLine
      let stns' = Stations { stations = ((filter (\x -> ((station_id x) /= numid)) (stations stns))), station_counter = (station_counter stns)}
      putStrLn "jest"
      return (stns')
  where
  notFounIdxError = "Nie ma takiego indeksu"


