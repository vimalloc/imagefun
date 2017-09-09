module Main where

import ConfigHelper
import RestAPI.Category

import Data.Either.Utils (forceEither)
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection)
import Network.Wai.Handler.Warp (run)

-- TODO hlint everything
-- TODO add logging (WIA middleware + reader)

main :: IO ()
main = do
    cp <- getConfigParser
    let connStr = forceEither $ parseDbString cp
    connection <- connectPostgreSQL connStr
    run 8080 $ app connection
