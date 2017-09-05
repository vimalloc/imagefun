{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RestAPI.Category where

import Models.Category
import Queries.Category
import ConfigHelper

import Control.Monad.IO.Class
import Data.Either.Utils
import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type CategoryApi = "categories" :> Get '[JSON] [CategoryRead]


-- TODO get a connection pool for postgres connections instead of making
--      one per request. This is just a dummy setup to test things work.
categoryServer :: Server CategoryApi
categoryServer = do
    cp <- liftIO getConfigParser
    let connStr = forceEither $ parseDbString cp
    connection <- liftIO $ connectPostgreSQL connStr
    categories <- liftIO $ runCategoryQuery connection categoriesQuery
    return categories


categoryApi :: Proxy CategoryApi
categoryApi = Proxy

app :: Application
app = serve categoryApi categoryServer
