module Main where

import  ConfigHelper
import  RestAPI.App
import  RestAPI.RestHelpers

import  Data.ConfigFile (get)
import  Data.Default.Class (def)
import  Data.Either.Utils (forceEither)
import  Data.Pool (createPool)
import  Database.PostgreSQL.Simple (connectPostgreSQL, close)
import  Network.Wai.Handler.Warp (run)
import  Network.Wai.Middleware.RequestLogger (mkRequestLogger, destination,
                                              Destination(Logger))
import  System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)

-- TODO change port and logger to be in config file as well

main :: IO ()
main = do
    cp <- getConfigParser
    let connStr      = forceEither $ parseDbString cp
    let numPools     = forceEither $ parseNumPools cp
    let connsPerPool = forceEither $ parseConnsPerPool cp
    let connTimeout  = forceEither $ parseConnTimeout cp
    pool    <- createPool (connectPostgreSQL connStr) close
                           numPools connTimeout connsPerPool
    logger  <- newStdoutLoggerSet defaultBufSize
    midware <- mkRequestLogger $ def { destination = Logger logger }
    run 8080 $ midware $ app (ReaderItems pool logger)
