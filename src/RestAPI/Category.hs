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
import           RestAPI.RestErrors
import           Queries.Category

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Except (ExceptT)
import           Database.PostgreSQL.Simple (Connection)
import qualified Network.Wai as NW
import qualified Opaleye as O
import qualified Servant as S
import           Servant ((:>), (:<|>) (..), (:~>))

-- TODO break everything that will need to go into multiple apis up into
--      a different file (like RestHandler, etc)


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
        connection <- ask
        liftIO $ O.runQuery connection categoriesQuery

    -- TODO run query here instead of cateboryById, and do another function to
    --      verify that exactly one result is found (like listToMaybe_
    getCategoryById :: Int -> RestHandler CategoryRead
    getCategoryById categoryId = do
        connection <- ask
        category <- liftIO $ categoryById connection categoryId
        case category of
            Just c  -> return c
            Nothing -> S.throwError notFoundErr
      where
        notFoundDetail = "Category " ++ (show categoryId) ++ " was not found."
        notFoundTitle  = "CategoryNotFound"
        notFoundErr    = errorToJSON 404 notFoundTitle notFoundDetail

-- TODO I understand why this is needed, but I don't understand how it
--      actually works at all.
readerTToExcept :: Connection -> RestHandler :~> S.Handler
readerTToExcept connection = S.NT (\r -> runReaderT r connection)

app ::Connection -> NW.Application
app conn = S.serve categoryApi $ S.enter (readerTToExcept conn) categoryServer
