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
import           Servant ((:>))

type CategoryApi = "categories" :> S.Get '[S.JSON] [CategoryRead]


-- TODO get a connection pool for postgres connections instead of making
--      one per request. This is just a dummy setup to test things work.
categoryServer :: S.Server CategoryApi
categoryServer = do
    cp <- liftIO getConfigParser
    let connStr = forceEither $ parseDbString cp
    connection <- liftIO $ connectPostgreSQL connStr
    categories <- liftIO $ runCategoryQuery connection categoriesQuery
    return categories


categoryApi :: S.Proxy CategoryApi
categoryApi = S.Proxy

app :: NW.Application
app = S.serve categoryApi categoryServer
