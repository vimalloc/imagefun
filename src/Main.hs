module Main where

import           ConfigHelper
import           RestAPI.App

import           Data.ConfigFile (get)
import           Data.Either.Utils (forceEither)
import           Data.Pool (createPool)
import qualified Database.PostgreSQL.Simple as PGS
import           Network.Wai.Handler.Warp (run)

-- TODO add logging (WIA middleware + reader)

main :: IO ()
main = do
    cp <- getConfigParser
    let connStr      = forceEither $ parseDbString cp
    let numPools     = forceEither $ parseNumPools cp
    let connsPerPool = forceEither $ parseConnsPerPool cp
    let connTimeout  = forceEither $ parseConnTimeout cp
    pool <- createPool (PGS.connectPostgreSQL connStr) PGS.close
                       numPools connTimeout connsPerPool
    run 8080 $ app pool
