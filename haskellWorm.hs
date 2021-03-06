import System.Directory
import Network.HTTP


getBookwormPage :: String -> String -> IO String
getBookwormPage host query = do
  let url = "http://" ++ host ++ "/cgi-bin/dbbindings.py/?query=" ++ query
  simpleHTTP (getRequest url) >>= getResponseBody


saveBookwormJSON :: String -> String -> String -> IO ()
saveBookwormJSON "" "" destination = saveBookwormJSON "benschmidt.org" "%7B%22database%22%3A%22federalist%22%2C%22plotType%22%3A%22barchart%22%2C%22method%22%3A%22return_json%22%2C%22search_limits%22%3A%7B%22word%22%3A%5B%22test%22%5D%7D%2C%22aesthetic%22%3A%7B%22x%22%3A%22WordCount%22%2C%22y%22%3A%22author%22%7D%2C%22counttype%22%3A%5B%22WordCount%22%5D%2C%22groups%22%3A%5B%22author%22%5D%7D" destination

saveBookwormJSON host query destination = do
  exists <- doesFileExist destination
  if not exists
    then do
        json <- getBookwormPage host query
        writeFile destination json
    else return ()

