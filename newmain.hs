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
  stop_time :: Duration, -- czas postoju na stacji w minutach
  travel_time :: Duration -- czas podróży do następnej stacji w minutach
} deriving (Show)

data Track = Track {
  track_id :: TrackId,
  track_name :: String,
  track_starts :: [Time], -- lista dokładnych momentów (data+godzina) wyruszenia pociągu z pierwszej stacji
  track_stations :: [TrackStation]
} deriving (Show)

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

instance Eq Edge where
  (==) = edgeEq

instance Eq PathCost where
  (==) = pathCostEq

--instance Ord Cost where
--  (<) = costOrd

instance Ord PathCost where
  (<) = pathCostOrd 

edgeEq :: Edge -> Edge -> Bool
edgeEq a b = (((src_node a) == (src_node b)) && ((dest_node a) == (dest_node b))) 

pathCostEq :: PathCost -> PathCost -> Bool
pathCostEq a b = ((nod_id a) == (nod_id b))

--costOrd :: Cost -> Cost -> Cost
--costOrd (Finite (ap,ac)) (Finite (bp,bc)) = ((ap < bp) || ((ap == bp) && (ac < bc)))
--costOrd Infty _ = False
--costOrd (Finite _) Infty = True


pathCostOrd :: PathCost -> PathCost -> Bool
pathCostOrd a b = (dist a) < (dist b)

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

-- algorithm :: Time -> Int -> StationId -> StationId -> [Track] -> [Array ?]
algorithm day max_p src_v dest_v tracks =
  map (algorithm_inst exp_track_stns dest_v max_p) source_exp_track_stations
  where
  exp_track_stns = foldl (++) [] (map expandTrack tracks)
  flat_exp_tracks_stns = map (\[v] -> v) exp_track_stns
  source_exp_track_stations = getSourceExpandedTrackStations flat_exp_tracks_stns day src_v

algorithm_inst exp_tracks dest_v max_p source_exp_track = 
  []
  where
  avail_exp_tracks = assignIdsToAll (filter (\track -> (any (\v -> (departure v) > (arrival source_exp_track)) track)) exp_tracks) 0
  flat_avail_exp_tracks = foldl (++) [] avail_exp_tracks
  graph = makeEdges avail_exp_tracks
  paths = dijkstra graph (node_id source_exp_track)




makeEdges :: [[ExpandedTrackStation]] -> [Edge]
makeEdges avail_exp_tracks = 
--  trace (show stns)
  track_edges ++ track_edges
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


genTrackTimetable :: StationId -> [[ExpandedTrackStation]] -> [(TrackId, Time, Time)]
genTrackTimetable _ [] = []
genTrackTimetable stn (fst:exp_tracks) =
  case find (\v -> (trck_id v) == stn) fst of
    Nothing -> genTrackTimetable stn exp_tracks
    Just v -> (trck_id v, arrival v, departure v):(genTrackTimetable stn exp_tracks)


generateTimetable :: StationId -> [Track] -> [(TrackId, Time, Time)]
generateTimetable _ [] = []
generateTimetable stn (fst:tracks) = 
  (genTrackTimetable stn exp_tracks) ++ (generateTimetable stn tracks)
  where
    exp_tracks = expandTrack fst



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

