{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RestAPI.Category where

import           Models.Category
import           Queries.Category
import           RestAPI.RestHelpers

import           Control.Exception (try, throw)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Class (lift)
import           Data.Pool (Pool, withResource)
import           Database.PostgreSQL.Simple (Connection, SqlError, sqlState,
                                             sqlErrorMsg)
import qualified Network.Wai as NW
import qualified Opaleye as O
import           Opaleye ((.==))
import qualified Servant as S
import           Servant ((:>), (:<|>) (..), (:~>))

-- TODO break everything that will need to go into multiple apis up into
--      a different file (like RestHandler, etc)
-- TODO consider making a seperate sub folder for each api, and then we
--      can break up exceptions, endpoint handlers, etc. (easier to view
--      code you need to look at if they are in different files)

type CategoryApi = "categories" :>
                     (
                     S.Get '[S.JSON] [CategoryRead]
                :<|> S.Capture "id" Int :> S.Get '[S.JSON] CategoryRead
                :<|> S.ReqBody '[S.JSON] CategoryWrite :> S.PostCreated '[S.JSON] CategoryRead
                :<|> S.Capture "id" Int :> S.ReqBody '[S.JSON] CategoryWrite :> S.Put '[S.JSON] CategoryRead
                :<|> S.Capture "id" Int :> S.DeleteNoContent '[S.JSON] S.NoContent
                     )

type RestHandler = ReaderT (Pool Connection) S.Handler

categoryApi :: S.Proxy CategoryApi
categoryApi = S.Proxy

categoryServer :: S.ServerT CategoryApi RestHandler
categoryServer = getAllCategories
            :<|> getCategory
            :<|> createCategory
            :<|> updateCategory
            :<|> deleteCategory
  where
    getAllCategories :: RestHandler [CategoryRead]
    getAllCategories = do
        pool <- ask
        conn <- getConnFromPool pool
        liftIO $ O.runQuery conn categoriesQuery

    getCategory :: Int -> RestHandler CategoryRead
    getCategory catId = do
        pool     <- ask
        conn     <- getConnFromPool pool
        category <- liftIO $ O.runQuery conn $ categoryByIdQuery catId
        case category of
            []  -> S.throwError $ categoryNotFound catId
            x:_ -> return x

    createCategory :: CategoryWrite -> RestHandler CategoryRead
    createCategory newCat = do
        pool   <- ask
        conn   <- getConnFromPool pool
        result <- liftIO (try (
                      fmap (!! 0) (O.runInsertManyReturning conn categoryTable
                                   [categoryToCategoryColumn newCat] id)
                  ) :: IO (Either SqlError CategoryRead))
        case result of
            Left  ex  -> case sqlState ex of
                            "23505"  -> S.throwError $ uniqueFailedError newCat
                            _       -> throw ex
            Right val -> return val

    updateCategory :: Int -> CategoryWrite -> RestHandler CategoryRead
    updateCategory catId newCat = do
        pool   <- ask
        conn   <- getConnFromPool pool
        result <- liftIO (try (
                      O.runUpdateReturning conn categoryTable
                      (\_ -> categoryToCategoryColumnId catId newCat)
                      (\cat -> categoryId cat .== O.pgInt4 catId)
                      id
                  ) :: IO (Either SqlError [CategoryRead]))
        case result of
            Left  ex  -> case sqlState ex of
                            "23505" -> S.throwError $ uniqueFailedError newCat
                            _       -> throw ex
            Right val -> case val of
                            []  -> S.throwError $ categoryNotFound catId
                            x:_ -> return x

    deleteCategory :: Int -> RestHandler S.NoContent
    deleteCategory catId = do
        pool <- ask
        conn <- getConnFromPool pool
        numDeleted <- liftIO $ O.runDelete conn categoryTable
                               (\cat -> categoryId cat .== O.pgInt4 catId)
        case numDeleted of
            0 -> S.throwError $ categoryNotFound catId
            1 -> return S.NoContent

categoryNotFound :: Int -> S.ServantErr
categoryNotFound id = encodeJSONError $ JSONError 404 "Category Not Found"
                                        ("Category " ++ show id ++ " was not found.")
uniqueFailedError :: CategoryWrite -> S.ServantErr
uniqueFailedError c = encodeJSONError err
  where
    err = JSONError 409 "Cagetory Exists"
                    ("The category '" ++ categoryName c ++ "' already exists")

restHandlerToSHandler :: Pool Connection -> RestHandler :~> S.Handler
restHandlerToSHandler pool = S.NT (`runReaderT` pool)

getConnFromPool :: Pool Connection -> RestHandler Connection
getConnFromPool pool = withResource pool return

app :: Pool Connection -> NW.Application
app pool = S.serve categoryApi $ S.enter (restHandlerToSHandler pool) categoryServer
