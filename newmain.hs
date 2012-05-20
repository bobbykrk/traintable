-- Algorytm Dijkstra wg pomysłu: http://pastebin.com/V9VMUfs3

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
import qualified Control.Exception as C


type StationId = Int
type TrackId = Int
type Time = UTCTime
-- w minutach
type Duration = Int
type Weigth = Int
type NodeId = Int


data Cost = Finite (Weigth, Weigth) | Infty deriving (Eq, Ord, Show) 

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
  track_id :: TrackId,
  track_name :: String,
  track_starts :: [Time], -- lista dokładnych momentów (data+godzina) wyruszenia pociągu z pierwszej stacji
  track_stations :: [TrackStation]
} --deriving (Show) --trzeba zakomentować

data Tracks = Tracks {
  track_counter :: Int,
  tracks :: [Track]
} deriving (Show)

data Edge = Edge {
  src_node :: NodeId,
  dest_node :: NodeId,
  change_weigth :: Weigth, -- waga krawędzi związana z przesiadką
  time_weigth :: Weigth -- waga krawędzi związana z czasem podróży (oczekiwania)
} deriving (Show)

data PathCost = PathCost {
  prev_node :: Int, -- id poprzedniego wierzchołka w celu odbudowy drogi
  nod_id :: Int, -- id wierzchołka
  dist :: Cost -- dotychczasowy koszt ścieżki
} deriving (Show)

data ExpandedTrackStation = ExpandedTrackStation {
  trck_id :: Int,
  st_id :: Int,
  arrival :: Time, -- czas wjazdu na stację w formacie posix
  departure :: Time, -- czas wyjazdu ze stacji w formacie posix
  node_id :: NodeId
} deriving (Show)

instance Show Station where
  show stn = printf "id: %d nazwa: %s" (station_id stn) (station_name stn)
  
instance Show Track where
  show tr = printf "id: %d nazwa: %s" (track_id tr) (track_name tr)

--  track_id :: TrackId,
--  track_name :: String,
--  track_starts :: [Time], -- lista dokładnych momentów (data+godzina) wyruszenia pociągu z pierwszej stacji
--  track_stations :: [TrackStation]
  
instance Eq Edge where
  (==) = edgeEq

instance Eq PathCost where
  (==) = pathCostEq

--instance Ord Cost where
--  (<) = costOrd

instance Ord PathCost where
  (<) = pathCostOrd 
  min a b =  if (dist a) < (dist b) then a else b

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
      putStrLn "Wyznaczenie trasy"
      res <- getRoute stns trs
      print res
      mainMenu stns trs
    "4" -> do
      putStrLn "Wygenerowanie rozkładu jazdy"
      sched <- generateSchedule stns trs
      --print sched
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
  putStrLn "została dodana nowa stacja"
  return stns'

--zmienia nazwę stacji  
editStation stns = do
  mapM_  (putStrLn . show) (stations stns)
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
        putStrLn "nazwa została zmieniona"
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
        putStrLn "stacja została usunięta"
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
            putStrLn "nazwa została zmieniona"
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
              putStrLn "daty zostały zmienione"
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
              putStrLn "nowa trasa kursu została wprowadzona"
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
  let Just x' = parseTime defaultTimeLocale "%F %R" x :: Maybe UTCTime;
  res <- C.try(print x')::IO (Either C.SomeException ())
  case res of
    Left err -> do putStrLn "Błędnie podana data!"
                   addDate dates
    Right q  -> do
      putStrLn addDateHelp
      sel <- getLine
      case sel of
        "1" -> do addDate (dates ++ [x'])
        "0" -> return (dates ++ [x'])
        _ -> do
          putStrLn "Nieznana komenda\n"
          addDate dates
  where
  addDateHelp = "naciśnij:\n\
                \1 - aby wprowadzić kolejną datę\n\  
                \0 - aby zakończyć wpisywanie dat"   

addDate2 = do                
  putStrLn "Podaj datę w formacie: rrrr-mm-dd"
  x <- getLine
  let Just x' = parseTime defaultTimeLocale "%F" x :: Maybe UTCTime;
  res <- C.try(print x')::IO (Either C.SomeException ())
  case res of
    Left err -> do putStrLn "Błędnie podana data!"
                   addDate2
    Right q  -> return x'
                
---------------------------------------
-- 3 Wyznaczenie trasy             
---------------------------------------
--getRoute stn trs
--algorithm day max_p src_v dest_v tracks
--day     data UTCTime
--max_p   Int
--src_v   StationId
--dest_v  StationId
--tracks  [Track]

getRoute stns trs = do
  day <- addDate2
  putStr "Lista stacji:"
  mapM_  (putStrLn . show) (stations stns)
  putStr "podaj id pierwszej stacji: "
  src_v <- getLine
  let src_v' =  read src_v :: Int
  putStr "podaj id stacji docelowej: "
  dest_v <- getLine
  let dest_v' =  read dest_v :: Int
  putStr "podaj max liczbę przesiadek: "
  max_p <- getLine
  let max_p' =  read max_p :: Int
  let res = algorithm day max_p' src_v' dest_v' (tracks trs)
  return res
  --print res
  
--  if stnid == [] || checkDigits stnid == False
--  then do
--    putStrLn notFounIdxError
--  else do
--    let numid =  read stnid :: Int
--    case (find (\v -> (station_id v == numid)) (stations stns)) of
--      Nothing -> do 
--        putStrLn notFounIdxError
--      Just found -> do 
--        let x = generateTimetable numid (tracks trs)
        --let x = sortTimeTable x
--        print x
--  where
--  notFounIdxError = "Nie ma takiego indeksu"



edgeEq :: Edge -> Edge -> Bool
edgeEq a b = (((src_node a) == (src_node b)) && ((dest_node a) == (dest_node b))) 

pathCostEq :: PathCost -> PathCost -> Bool
pathCostEq a b = ((nod_id a) == (nod_id b))

--costOrd :: Cost -> Cost -> Cost
--costOrd (Finite (ap,ac)) (Finite (bp,bc)) = ((ap < bp) || ((ap == bp) && (ac < bc)))
--costOrd Infty _ = False
--costOrd (Finite _) Infty = True

pathCostOrd :: PathCost -> PathCost -> Bool
pathCostOrd a b = trace "xxx" ((dist a) < (dist b))

sumCosts :: Cost -> Cost -> Cost
sumCosts (Finite (wpa,wca)) (Finite (wpb,wcb)) = Finite ((wpa+wpb),(wca+wcb))
sumCosts _ _ = Infty

addSecondsToUTCTime secs time = posixSecondsToUTCTime ((utcTimeToPOSIXSeconds time) + (realToFrac secs :: POSIXTime) )

diffUTCTimeInSecs timea timeb = floor $ toRational $ (utcTimeToPOSIXSeconds timea) - (utcTimeToPOSIXSeconds timeb)

expandTrackInst [] _ _ _ = []
expandTrackInst (fst:track_stns) beg tck_id start_time = 
  (ExpandedTrackStation {trck_id = tck_id,
    st_id = (stn_id fst),
    arrival = (addSecondsToUTCTime (beg * 60) start_time), 
    departure = (addSecondsToUTCTime ((beg + (stop_time fst))* 60)  start_time),
    node_id = 0
  }):expandTrackInst track_stns (beg+ (stop_time fst) + (travel_time fst)) tck_id start_time

assignIdToTrack [] _ = []
assignIdToTrack (fst:trk) n =
  (fst{node_id = n}):assignIdToTrack trk (n+1)

assignIdsToAll [] _ = []
assignIdsToAll (fst:exp_tracks) n = (assignIdToTrack fst n):(assignIdsToAll exp_tracks (n+(length fst)))

expandTrack :: Track -> [[ExpandedTrackStation]]
expandTrack track = 
  let m = map (expandTrackInst (track_stations track) 0 (track_id track) ) (track_starts track)
  in assignIdsToAll m 0

-- pobiera wszystkie wierzchołki źródłowe v z danego dnia (time)
getSourceExpandedTrackStations flat_exp_tracks time v_id =
  filter (\fst -> (((arrival fst) > today) && ((departure fst) < tomorrow) && (v_id == (st_id fst)))) flat_exp_tracks
  where
  UTCTime d t = time
  today = UTCTime d 0
  tomorrow =  addSecondsToUTCTime (60*60*24) today

--makeGraph source_v exp_tracks
--  where
  -- wszystkie kursy, które mają szanse być w grafie
--  avail_exp_tracks = filter (\track -> (any (\v -> (departure v) > (arrival source_v)) track)) exp_tracks

--algorithm :: Time -> Int -> StationId -> StationId -> [Track] -> [[ExpandedTrackStation]]
algorithm day max_p src_v dest_v tracks =
  --trace (show source_exp_track_stations) 
  (map (algorithm_inst exp_track_stns dest_v max_p) source_exp_track_stations)
  --map (trace (show exp_track_stns ) (algorithm_inst exp_track_stns dest_v max_p)) source_exp_track_stations
  where
  exp_track_stns = assignIdsToAll (foldl (++) [] (map expandTrack tracks)) 0
  flat_exp_tracks_stns = foldl (++) [] exp_track_stns
  --flat_exp_tracks_stns = map (\[v] -> v) exp_track_stns
  source_exp_track_stations = getSourceExpandedTrackStations flat_exp_tracks_stns day src_v

getPathNodes :: NodeId -> NodeId -> [PathCost] -> Maybe [NodeId]
getPathNodes src_nd dst_nd paths =
  if src_nd == dst_nd 
    then
      Just [src_nd]
    else
      case v of
        Nothing -> Nothing
        Just n -> 
          if dist n == Infty 
            then Nothing
            else
              case rest of
                Nothing -> Just [(nod_id n)]              
                Just p -> Just(p ++ [(nod_id n)])
              where
                rest = (getPathNodes src_nd (prev_node n) paths)
              --Just (rest ++ [(nod_id n)])
              --where
                --Just rest = (getPathNodes src_nd (prev_node n) paths)
                --case (getPathNodes src_nd (prev_node n) paths) of
                  --Nothing ->
                  --Just n ->
      where
        v = find (\u -> (nod_id u) == dst_nd) paths



buildPath ::  [NodeId] -> [ExpandedTrackStation] -> [ExpandedTrackStation]
buildPath paths exp_tracks = 
  filter (\track -> (any (\v -> v == (node_id track)) paths)) exp_tracks


algorithm_inst exp_tracks dest_v max_p source_exp_track = 
  --trace (show pathNodes) 
  buildPaths pathNodes flat_avail_exp_tracks
--    case trace (show pathNodes) pathNodes of
--    Nothing -> []
    --Just pth -> buildPath pth flat_avail_exp_tracks
--    Just pth -> trace ((show pth)++(show flat_avail_exp_tracks)) (buildPath pth flat_avail_exp_tracks)
  where
  avail_exp_tracks = filter (\track -> (any (\v -> (departure v) > (arrival source_exp_track)) track)) exp_tracks
  flat_avail_exp_tracks = foldl (++) [] avail_exp_tracks
  --dest_v' = trace (show flat_avail_exp_tracks) (map node_id ( (filter (\v -> (st_id v) == dest_v) flat_avail_exp_tracks)))
  dest_v' = map node_id ( (filter (\v -> (st_id v) == dest_v) flat_avail_exp_tracks))
  graph = makeEdges avail_exp_tracks
  --paths = dijkstra graph (node_id source_exp_track)
  --paths = trace (show graph) (dijkstra graph (node_id source_exp_track))

  paths = trace (show graph ++ show dest_v')  (map (\v -> dijkstra graph v) dest_v')

  --pathNodes = getPathNodes (node_id source_exp_track) dest_v' paths
  --pathNodes = trace ((show paths)++(show(node_id source_exp_track))++(show dest_v')) (getPathNodes (node_id source_exp_track) dest_v' paths)
--  pathNodes = map (\v -> getPathNodes (node_id source_exp_track) dest_v' v) paths
  pathNodes = trace ((show paths)++(show(node_id source_exp_track))++(show dest_v')) [getPathNodes (node_id source_exp_track) d_v v | d_v <- dest_v', v <- paths]
  
buildPaths [] _ = []
buildPaths (x:xs) flat_avail_exp_tracks = 
  case x of
    Nothing -> buildPaths xs flat_avail_exp_tracks 
    Just pth -> (buildPath pth flat_avail_exp_tracks ):(buildPaths xs flat_avail_exp_tracks )
  
  
--algfun [x:xs] = 
  --case trace (show pathNodes) pathNodes of
    --Nothing -> [] ++
    --Just pth -> buildPath pth flat_avail_exp_tracks
    --Just pth -> trace ((show pth)++(show flat_avail_exp_tracks)) (buildPath pth flat_avail_exp_tracks)
  
makeEdges :: [[ExpandedTrackStation]] -> [Edge]
makeEdges avail_exp_tracks = 
--  trace (show stns)
  track_edges ++ station_edges
  where
    flat_avail_exp_tracks = foldl (++) [] avail_exp_tracks
    stns = (nub . (map (\v -> st_id v) )) flat_avail_exp_tracks  
    station_edges = foldl (++) [] (map (makeStationEdge flat_avail_exp_tracks) stns)
    track_edges = foldl (++) [] (map makeTrackEdge avail_exp_tracks)


makeStationEdge flat_avail_exp_tracks stn_id =
  [Edge {src_node = (node_id x),dest_node = (node_id y), change_weigth = 1, time_weigth = (diffUTCTimeInSecs (departure y) (arrival x)) }|x <- flat_avail_exp_tracks, y <- flat_avail_exp_tracks,  (departure y) > (arrival x), (node_id x) /= (node_id y), (st_id x) == stn_id, (st_id y) == stn_id]

nodes :: [Edge] -> [NodeId]
nodes edges = nub (foldl (++) [] (map (\v -> [src_node v, dest_node v]) edges))


findEInGraph _ [] = Nothing
findEInGraph (u,v) (fst:edges)
  | (u == (src_node fst) && v == (dest_node fst)) = Just ((change_weigth fst), (time_weigth fst))
  | otherwise = findEInGraph (u,v) edges

findE :: (NodeId, NodeId) -> [Edge] -> Cost
findE edge = maybe Infty Finite . findEInGraph edge



remove :: Eq a => a -> [a] -> [a]
remove = flip (\\) . flip (:) []

sumPaths :: PathCost -> PathCost -> PathCost
sumPaths a b =  PathCost {prev_node = (nod_id a), nod_id = (nod_id b), dist = (sumCosts (dist a) (dist b))}

dijkstra :: [Edge] -> NodeId -> [PathCost]
dijkstra graph src = 
  addPaths [PathCost{prev_node = src, nod_id = node, dist = (findE (src, node) graph)}| node <- nodes graph] []
  where
    addPaths :: [PathCost] -> [PathCost] -> [PathCost]
    addPaths [] ac = ac
    addPaths ps ac =
      addPaths (map relax (remove minp ps)) (minp : ac)
      where
        minp = minimum ps
        relax pc = if (dist pc) < nc then pc else (PathCost {prev_node = (nod_id minp), nod_id = (nod_id pc), dist = nc})
          where
            nc = min (dist pc) (sumCosts (dist minp) (findE (nod_id minp, nod_id pc) graph ) )

makeTrackEdge [] = []
makeTrackEdge [x] = []
makeTrackEdge (fstel:secel:avail_exp_track) = 
  (Edge {src_node = (node_id fstel),
         dest_node = (node_id secel), 
         change_weigth = 0, 
         time_weigth = (diffUTCTimeInSecs (arrival secel) (departure fstel))
         }):makeTrackEdge(secel:avail_exp_track)

---------------------------------------
-- 4 Wygenerowanie rozkładu jazdy            
---------------------------------------

generateSchedule stns trs = do
  putStrLn "Wygenerowanie rozkładu jazdy"
  mapM_  (putStrLn . show) (stations stns)
  putStr "podaj id stacji: "
  stnid <- getLine
  if stnid == [] || checkDigits stnid == False
  then do
    putStrLn notFounIdxError
  else do
    let numid =  read stnid :: Int
    case (find (\v -> (station_id v == numid)) (stations stns)) of
      Nothing -> do 
        putStrLn notFounIdxError
      Just found -> do 
        let x = generateTimetable numid (tracks trs)
        let x' = sortTimeTable x
        --print x'
        --show stn = printf "id: %d nazwa: %s" (station_id stn) (station_name stn)
        mapM_  (putStrLn . show) x'
        --putStrLn "----"
       -- printf "%d %s %s" x'
  where
  notFounIdxError = "Nie ma takiego indeksu"


genTrackTimetable :: StationId -> [[ExpandedTrackStation]] -> [(TrackId, Time, Time)]
genTrackTimetable _ [] = []
genTrackTimetable stn (fst:exp_tracks) =
  case find (\v -> (st_id v) == stn) fst of
    Nothing -> genTrackTimetable stn exp_tracks
    --Just v -> (trck_id v, arrival v, departure v):(genTrackTimetable stn exp_tracks)
    Just v -> (trck_id v, arrival v, departure v):(genTrackTimetable stn exp_tracks)

generateTimetable :: StationId -> [Track] -> [(TrackId, Time, Time)]
generateTimetable _ [] = []
generateTimetable stn (fst:tracks) = 
  (genTrackTimetable stn exp_tracks) ++ (generateTimetable stn tracks)
  where
    exp_tracks = expandTrack fst

sortTimeTable :: [(TrackId, Time, Time)] -> [(TrackId, Time, Time)]
sortTimeTable = sortBy (\(tida,aa,da) (tidb,ab,db) -> (compare (aa,da)  (ab,db)))

