module ConfigHelper where

import           Control.Monad (liftM5)
import qualified Data.ConfigFile as CP
import           Data.Either.Utils (forceEither)
import           Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS

parseDbString :: CP.ConfigParser -> Either CP.CPError BS.ByteString
parseDbString cp = liftM5 buildDbString (getDb "host") (getDb "port")
                                        (getDb "username") (getDb "password")
                                        (getDb "dbname")
  where
    getDb = CP.get cp "POSTGRES"

    buildDbString :: String -> String -> String -> String -> String -> BS.ByteString
    buildDbString host port user password dbname = BS.pack connStr
      where
        connTemplate = "host=%s port=%s user=%s password=%s dbname=%s"
        connStr      = printf connTemplate host port user password dbname

getConfigParser :: IO CP.ConfigParser
getConfigParser = fmap forceEither (CP.readfile CP.emptyCP "imagefun.cfg")
