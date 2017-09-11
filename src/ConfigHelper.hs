module ConfigHelper where

import           Control.Monad (liftM5)
import qualified Data.ConfigFile as CP
import           Data.Either.Utils (forceEither)
import           Data.Time.Clock (NominalDiffTime)
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

parseNumPools :: CP.ConfigParser -> Either CP.CPError Int
parseNumPools cp = CP.get cp "POSTGRES" "number_of_pools"

parseConnsPerPool :: CP.ConfigParser -> Either CP.CPError Int
parseConnsPerPool cp = CP.get cp "POSTGRES" "connections_per_pool"

parseConnTimeout :: CP.ConfigParser -> Either CP.CPError NominalDiffTime
parseConnTimeout cp = fromInteger <$> CP.get cp "POSTGRES" "connection_timeout"

getConfigParser :: IO CP.ConfigParser
getConfigParser = forceEither <$> CP.readfile CP.emptyCP "imagefun.cfg"
