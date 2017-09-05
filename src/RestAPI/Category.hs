{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RestAPI.Category where

import           ConfigHelper
import           Models.Category
import           Queries.Category

import           Control.Monad.IO.Class (liftIO)
import           Data.Either.Utils (forceEither)
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import qualified Network.Wai as NW
import qualified Servant as S
import           Servant ((:>), (:<|>)(..))

type CategoryApi = "categories" :> S.Get '[S.JSON] [CategoryRead]
              :<|> "categories" :> S.Capture "categoryId" Int :> S.Get '[S.JSON] CategoryRead

categoryApi :: S.Proxy CategoryApi
categoryApi = S.Proxy

-- TODO get a connection pool for postgres connections instead of making
--      one per request. This is just a dummy setup to test things work.
--
categoryServer :: S.Server CategoryApi
categoryServer = getAllCategories
            :<|> getCategoryById

getAllCategories :: S.Handler [CategoryRead]
getAllCategories = do
    cp <- liftIO getConfigParser
    let connStr = forceEither $ parseDbString cp
    connection <- liftIO $ connectPostgreSQL connStr
    categories <- liftIO $ runCategoryQuery connection categoriesQuery
    return categories


-- TODO return 404 with error if category doesn't exist
-- TODO this query should just return a Maybe with the Just being the single
--      result fo the query, instead of returning a list and making us deal
--      with it
getCategoryById :: Int -> S.Handler CategoryRead
getCategoryById categoryId = do
    cp <- liftIO getConfigParser
    let connStr = forceEither $ parseDbString cp
    connection <- liftIO $ connectPostgreSQL connStr
    category <- liftIO $ runCategoryQuery connection $ categoryByIdQuery categoryId
    return $ category !! 0


app :: NW.Application
app = S.serve categoryApi categoryServer
