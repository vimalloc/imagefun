module ConfigHelper where

import           Control.Monad
import           Data.ConfigFile
import           Data.Either.Utils
import           Text.Printf
import qualified Data.ByteString.Char8 as BS

parseDbString :: ConfigParser -> Either CPError BS.ByteString
parseDbString cp = liftM5 buildDbString (getDb "host") (getDb "port")
                                        (getDb "username") (getDb "password")
                                        (getDb "dbname")
  where
    getDb = get cp "POSTGRES"

    buildDbString :: String -> String -> String -> String -> String -> BS.ByteString
    buildDbString host port user password dbname = BS.pack connStr
      where
        connTemplate = "host=%s port=%s user=%s password=%s dbname=%s"
        connStr      = printf connTemplate host port user password dbname

getConfigParser :: IO ConfigParser
getConfigParser = readfile emptyCP "imagefun.cfg" >>= return . forceEither
