module RestAPI.Category.Errors where

import           Models.Category
import           RestAPI.RestHelpers

import qualified Servant as S

categoryNotFound :: Int -> S.ServantErr
categoryNotFound id = encodeJSONError err
  where
    err = JSONError 404 "Category Not Found"
                    $ "Category " ++ show id ++ " was not found."

uniqueFailedError :: CategoryWrite -> S.ServantErr
uniqueFailedError c = encodeJSONError err
  where
    err = JSONError 409 "Cagetory Exists"
                    $ "The category '" ++ categoryName c ++ "' already exists"
