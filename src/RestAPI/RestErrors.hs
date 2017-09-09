module RestAPI.RestErrors where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import           Data.CaseInsensitive  (mk)
import qualified Servant as S
import           Text.Printf (printf)

errorToJSON :: Int -> String -> String -> S.ServantErr
errorToJSON code title detail = err {S.errBody = jsonBody,
                                     S.errHeaders = [jsonHeader]}
  where
    err          = getErrorFromCode code
    jsonTemplate = "{\"status\": %d, \"title\": \"%s\", \"detail\": \"%s\"}"
    jsonBody     = LazyBS.pack $ printf jsonTemplate code title detail
    jsonHeader   = ((mk $ BS.pack "Content-Type"),
                    (BS.pack "application/json;charset=utf-8"))

-- Non-exhaustive. Trivial to add more, just only adding the ones I
-- am currently using.
getErrorFromCode :: Int -> S.ServantErr
getErrorFromCode 400 = S.err400
getErrorFromCode 404 = S.err404
