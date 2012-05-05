import Control.Monad

data Elem = Elem {
  name :: String,
  value :: String
  } deriving (Show)

getElem = do 
  []

main = do
  let ls = []
  lst <- mainLoop ls
  return (lst)



mainLoop ls = do
  opt <- getLine
  putStrLn "w mainLoop"
  case opt of
    "1" -> do
      putStrLn "w 1"
      nme <- getLine
      val <- getLine
      let lst' = (Elem {name = nme, value = val}):ls 
     -- lst ++ [(Elem {name = "asd", value = "qwe"})]
      mainLoop lst'
    _ -> do
      putStrLn "wychodzi"
      return (ls)
