{-# LANGUAGE DeriveGeneric #-}

module RestAPI.RestHelpers where

import           Data.Aeson (ToJSON, encode)
import           Data.ByteString.Char8 (pack)
import           Data.CaseInsensitive  (mk)
import           GHC.Generics (Generic)
import qualified Servant as S

data JSONError = JSONError
    { statusCode :: Int
    , title :: String
    , detail :: String
    } deriving (Generic, Show)

instance ToJSON JSONError

encodeJSONError :: JSONError -> S.ServantErr
encodeJSONError jsonError = err {S.errBody = jsonBody, S.errHeaders = [jsonHeader]}
  where
    err        = getErrorFromCode $ statusCode jsonError
    jsonBody   = encode jsonError
    jsonHeader = ((mk $ pack "Content-Type"),
                  (pack "application/json;charset=utf-8"))

-- Non-exhaustive. Trivial to add more, just only adding the ones I
-- am currently using.
getErrorFromCode :: Int -> S.ServantErr
getErrorFromCode 400 = S.err400
getErrorFromCode 404 = S.err404
getErrorFromCode 500 = S.err500

-- The bottom case for this (multiple results) should never be hit. The only
-- time this function should be used in in conjunction with queries that we
-- know should only return a signle result from the database (due to a unique
-- constraint). If an error is raised there, it is probably due to an incorrect
-- schema setup on postgres, and as such we will throw a special 500 error
-- error instead of the default error passed in that we raise if the object
-- was not found in the query at all.
oneOrError :: [a] -> S.ServantErr -> S.Handler a
oneOrError [] err   = S.throwError err
oneOrError (x:[]) _ = return x
oneOrError (x:xs) _ = S.throwError $ encodeJSONError moreThenOneErr
  where
    moreThenOneErr = JSONError 500 "Bad Query Result"
                     "More then one result was returned as part of this SQL query"
