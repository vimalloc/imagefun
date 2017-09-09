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
import qualified Servant as S
import           Servant ((:>), (:<|>) (..), (:~>))

-- TODO break everything that will need to go into multiple apis up into
--      a different file (like RestHandler, etc)


-- TODO Do the break combinators up thing here to save space
type CategoryApi = "categories" :> S.Get '[S.JSON] [CategoryRead]
              :<|> "categories" :> S.Capture "categoryId" Int :> S.Get '[S.JSON] CategoryRead

-- TODO get a connection pool instead of single connection in the ReaderT monad
type RestHandler = ReaderT Connection S.Handler

categoryApi :: S.Proxy CategoryApi
categoryApi = S.Proxy

categoryServer :: S.ServerT CategoryApi RestHandler
categoryServer = getAllCategories
            :<|> getCategoryById
  where
    getAllCategories :: RestHandler [CategoryRead]
    getAllCategories = do
        conn <- ask
        liftIO $ O.runQuery conn categoriesQuery

    getCategoryById :: Int -> RestHandler CategoryRead
    getCategoryById categoryId = do
        conn     <- ask
        category <- liftIO $ O.runQuery conn $ categoryByIdQuery categoryId
        lift $ oneOrError category notFoundErr
      where
        notFoundErr = errorToJSON 404 "CategoryNotFound"
                      $ "Category " ++ (show categoryId) ++ " was not found."

-- TODO I understand why this is needed, but I don't understand how it
--      actually works at all.
restHandlerToExcept :: Connection -> RestHandler :~> S.Handler
restHandlerToExcept connection = S.NT (\r -> runReaderT r connection)

app ::Connection -> NW.Application
app conn = S.serve categoryApi $ S.enter (restHandlerToExcept conn) categoryServer
