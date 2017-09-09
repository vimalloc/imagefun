{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RestAPI.Category where

import           Models.Category
import           Queries.Category
import           RestAPI.RestHelpers

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Class (lift)
import           Database.PostgreSQL.Simple (Connection)
import qualified Network.Wai as NW
import qualified Opaleye as O
import           Opaleye ((.==))
import qualified Servant as S
import           Servant ((:>), (:<|>) (..), (:~>))

-- TODO break everything that will need to go into multiple apis up into
--      a different file (like RestHandler, etc)


type CategoryApi = "categories" :>
                     (
                     S.Get '[S.JSON] [CategoryRead]
                :<|> S.Capture "id" Int :> S.Get '[S.JSON] CategoryRead
                :<|> S.Capture "id" Int :> S.DeleteNoContent '[S.JSON] S.NoContent
                     )

-- TODO get a connection pool instead of single connection in the ReaderT monad
type RestHandler = ReaderT Connection S.Handler

categoryApi :: S.Proxy CategoryApi
categoryApi = S.Proxy

categoryServer :: S.ServerT CategoryApi RestHandler
categoryServer = getAllCategories
            :<|> getCategoryById
            :<|> deleteCategoryById
  where
    getAllCategories :: RestHandler [CategoryRead]
    getAllCategories = do
        conn <- ask
        liftIO $ O.runQuery conn categoriesQuery

    getCategoryById :: Int -> RestHandler CategoryRead
    getCategoryById categoryId = do
        conn     <- ask
        category <- liftIO $ O.runQuery conn $ categoryByIdQuery categoryId
        lift $ oneOrError category $ categoryNotFound categoryId

    deleteCategoryById :: Int -> RestHandler S.NoContent
    deleteCategoryById id = do
        conn <- ask
        numDeleted <- liftIO $ O.runDelete conn categoryTable
                               (\cat -> (categoryId cat) .== (O.pgInt4 id))
        case numDeleted of
            0 -> S.throwError $ categoryNotFound id
            1 -> return S.NoContent

categoryNotFound :: Int -> S.ServantErr
categoryNotFound id = errorToJSON 404 "CategoryNotFound"
                      $ "Category " ++ (show id) ++ " was not found."

-- TODO I understand why this is needed, but I don't understand how it
--      actually works at all.
restHandlerToExcept :: Connection -> RestHandler :~> S.Handler
restHandlerToExcept connection = S.NT (\r -> runReaderT r connection)

app ::Connection -> NW.Application
app conn = S.serve categoryApi $ S.enter (restHandlerToExcept conn) categoryServer
