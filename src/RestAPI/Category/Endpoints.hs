{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RestAPI.Category.Endpoints where

import           Models.Category
import           Queries.Category
import           RestAPI.Category.Errors
import           RestAPI.RestHelpers

import           Control.Exception (try, throw)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Class (lift)
import           Database.PostgreSQL.Simple ( SqlError, sqlState, sqlErrorMsg)
import qualified Opaleye as O
import           Opaleye ((.==))
import qualified Servant as S
import           Servant ((:>), (:<|>) (..), (:~>))

type CategoryApi = "categories" :>
                     (
                     S.Get '[S.JSON] [CategoryRead]
                :<|> S.Capture "id" Int :> S.Get '[S.JSON] CategoryRead
                :<|> S.ReqBody '[S.JSON] CategoryWrite :> S.PostCreated '[S.JSON] CategoryRead
                :<|> S.Capture "id" Int :> S.ReqBody '[S.JSON] CategoryWrite :> S.Put '[S.JSON] CategoryRead
                :<|> S.Capture "id" Int :> S.DeleteNoContent '[S.JSON] S.NoContent
                     )


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
        conn <- getConn
        liftIO $ O.runQuery conn categoriesQuery

    getCategory :: Int -> RestHandler CategoryRead
    getCategory catId = do
        conn     <- getConn
        category <- liftIO $ O.runQuery conn $ categoryByIdQuery catId
        case category of
            []  -> S.throwError $ categoryNotFound catId
            x:_ -> return x

    createCategory :: CategoryWrite -> RestHandler CategoryRead
    createCategory newCat = do
        conn   <- getConn
        result <- liftIO $ try $
                      head <$> O.runInsertManyReturning conn categoryTable
                               [categoryToCategoryColumn newCat] id
        case result of
            Left  ex  -> case sqlState ex of
                            "23505" -> S.throwError $ uniqueFailedError newCat
                            _       -> throw ex
            Right val -> return val

    updateCategory :: Int -> CategoryWrite -> RestHandler CategoryRead
    updateCategory catId newCat = do
        conn   <- getConn
        result <- liftIO $ try $
                      O.runUpdateReturning conn categoryTable
                      (\_ -> categoryToCategoryColumnId catId newCat)
                      (\cat -> categoryId cat .== O.pgInt4 catId)
                      id
        case result of
            Left  ex  -> case sqlState ex of
                            "23505" -> S.throwError $ uniqueFailedError newCat
                            _       -> throw ex
            Right val -> case val of
                            []  -> S.throwError $ categoryNotFound catId
                            x:_ -> return x

    deleteCategory :: Int -> RestHandler S.NoContent
    deleteCategory catId = do
        conn <- getConn
        numDeleted <- liftIO $ O.runDelete conn categoryTable
                               (\cat -> categoryId cat .== O.pgInt4 catId)
        case numDeleted of
            0 -> S.throwError $ categoryNotFound catId
            1 -> return S.NoContent
