module Bookworm where

import System.Directory
import Network.HTTP

--- For hashing Bookworm dicts

import qualified Data.ByteString as B
import Data.Text.Encoding as E
import qualified Data.Text as T
import Crypto.Hash (digestToHexByteString, Digest, hash, SHA1)

-------------------------
-- SHA1 Bookworm codes --
-------------------------

--- I encode each query as a SHA1 hex to identify it consistently.
-- Sidenote: oh my God Haskell sometimes seems completely ridiculous.
-- According Stack Overflow, this is the most efficient to encode
-- a string as SHA1! It takes four separate module imports!
-- Will it work on unicode? In theory...

sha1Hex :: B.ByteString -> B.ByteString
sha1Hex s = digestToHexByteString (hash s :: Digest SHA1)

encodeSHA1 :: String -> String
encodeSHA1 s = do
  T.unpack $ E.decodeUtf8 $ sha1Hex $ E.encodeUtf8 $ T.pack s

-------------------------
--- Save JSON to disk ---
-------------------------

getBookwormPage :: String -> String -> IO String
getBookwormPage host query = do
  let url = "http://" ++ host ++ "/cgi-bin/dbbindings.py/?query=" ++ query
  simpleHTTP (getRequest url) >>= getResponseBody


-- Enter the IO 
saveBookwormJSON :: String -> String -> String -> IO ()
saveBookwormJSON "" "" destination = saveBookwormJSON "benschmidt.org" "%7B%22database%22%3A%22federalist%22%2C%22plotType%22%3A%22barchart%22%2C%22method%22%3A%22return_json%22%2C%22search_limits%22%3A%7B%22word%22%3A%5B%22test%22%5D%7D%2C%22aesthetic%22%3A%7B%22x%22%3A%22WordCount%22%2C%22y%22%3A%22author%22%7D%2C%22counttype%22%3A%5B%22WordCount%22%5D%2C%22groups%22%3A%5B%22author%22%5D%7D" destination

saveBookwormJSON host query destination = do
  exists <- doesFileExist destination
  if not exists
    then do
        json <- getBookwormPage host query
        writeFile destination json
    else return ()

quickSave :: String -> IO ()
quickSave code = do
  let hash = encodeSHA1 code
  saveBookwormJSON "localhost" (urlEncode code) ("json/" ++ hash ++ ".json")
  
