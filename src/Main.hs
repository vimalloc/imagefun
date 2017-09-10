module Main where

import           ConfigHelper
import           RestAPI.Category

import           Data.Either.Utils (forceEither)
import           Data.Pool (createPool)
import qualified Database.PostgreSQL.Simple as PGS
import           Network.Wai.Handler.Warp (run)

-- TODO hlint everything
-- TODO add logging (WIA middleware + reader)
-- TODO change pool timeout and whatnot info to config file (and increase
--      timeout of resource in the pool to match postgres config, bigger is
--      probably better)

main :: IO ()
main = do
    cp <- getConfigParser
    let connStr = forceEither $ parseDbString cp
    pool <- createPool (PGS.connectPostgreSQL connStr) PGS.close 1 (5*60) 5
    run 8080 $ app pool
