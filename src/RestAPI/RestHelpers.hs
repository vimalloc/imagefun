{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RestAPI.RestHelpers where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ask, ReaderT)
import           Database.PostgreSQL.Simple (Connection)
import           Data.Aeson (ToJSON, encode)
import           Data.ByteString.Char8 (pack)
import           Data.CaseInsensitive  (mk)
import           Data.Pool (Pool, withResource)
import           GHC.Generics (Generic)
import qualified Servant as S
import           System.Log.FastLogger (LoggerSet, LogStr, pushLogStrLn)

type ConnectionPool = Pool Connection

data ReaderItems = ReaderItems
    { getPool   :: ConnectionPool
    , getLogger :: LoggerSet
    }

type RestHandler = ReaderT ReaderItems S.Handler

data JSONError = JSONError
    { statusCode :: Int
    , title :: String
    , detail :: String
    } deriving (Generic, Show)

instance ToJSON JSONError


getConn :: RestHandler Connection
getConn = ask >>= (`withResource` return) . getPool

logInfo :: LogStr -> RestHandler ()
logInfo msg = ask >>= liftIO . (`pushLogStrLn` msg) . getLogger

encodeJSONError :: JSONError -> S.ServantErr
encodeJSONError jsonError = err {S.errBody = jsonBody, S.errHeaders = [jsonHeader]}
  where
    err        = getErrorFromCode $ statusCode jsonError
    jsonBody   = encode jsonError
    jsonHeader = (mk $ pack "Content-Type", pack "application/json;charset=utf-8")

-- Non-exhaustive. Trivial to add more, just only adding the ones I
-- am currently using.
getErrorFromCode :: Int -> S.ServantErr
getErrorFromCode 400 = S.err400
getErrorFromCode 404 = S.err404
getErrorFromCode 409 = S.err409
getErrorFromCode 500 = S.err500
