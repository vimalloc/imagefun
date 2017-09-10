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
import           Data.Pool (Pool, withResource)
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
                :<|> S.ReqBody '[S.JSON] CategoryWrite :> S.Post '[S.JSON] CategoryRead
                :<|> S.Capture "id" Int :> S.DeleteNoContent '[S.JSON] S.NoContent
                     )

type RestHandler = ReaderT (Pool Connection) S.Handler

categoryApi :: S.Proxy CategoryApi
categoryApi = S.Proxy

categoryServer :: S.ServerT CategoryApi RestHandler
categoryServer = getAllCategories
            :<|> getCategoryById
            :<|> createNewCategory
            :<|> deleteCategoryById
  where
    getAllCategories :: RestHandler [CategoryRead]
    getAllCategories = do
        pool <- ask
        conn <- getConnFromPool pool
        liftIO $ O.runQuery conn categoriesQuery

    getCategoryById :: Int -> RestHandler CategoryRead
    getCategoryById id = do
        pool     <- ask
        conn     <- getConnFromPool pool
        category <- liftIO $ O.runQuery conn $ categoryByIdQuery id
        lift . oneOrError category $ categoryNotFound id

    -- TODO get primary key back from original query instead of having
    --      to run two seperate queries here
    -- TODO catch and handle unique key failure
    createNewCategory :: CategoryWrite -> RestHandler CategoryRead
    createNewCategory newCat = do
        pool <- ask
        conn <- getConnFromPool pool
        let category = categoryToCategoryColumn newCat
        liftIO $ O.runInsertMany conn categoryTable [category]
        categories <- liftIO $ O.runQuery conn $ categoryByNameQuery (categoryName newCat)
        return $ categories !! 0

    deleteCategoryById :: Int -> RestHandler S.NoContent
    deleteCategoryById id = do
        pool <- ask
        conn <- getConnFromPool pool
        numDeleted <- liftIO $ O.runDelete conn categoryTable
                               (\cat -> (categoryId cat) .== (O.pgInt4 id))
        case numDeleted of
            0 -> S.throwError $ categoryNotFound id
            1 -> return S.NoContent

categoryNotFound :: Int -> S.ServantErr
categoryNotFound id = encodeJSONError $ JSONError 404 "Category Not Found"
                                        ("Category " ++ (show id) ++ " was not found.")

restHandlerToSHandler :: (Pool Connection) -> RestHandler :~> S.Handler
restHandlerToSHandler pool = S.NT (\r -> runReaderT r pool)

getConnFromPool :: (Pool Connection) -> RestHandler Connection
getConnFromPool pool = withResource pool return

app :: (Pool Connection) -> NW.Application
app pool = S.serve categoryApi $ S.enter (restHandlerToSHandler pool) categoryServer
