{-# LANGUAGE TypeOperators #-}

module RestAPI.App where

import           RestAPI.Category.Endpoints
import           RestAPI.RestHelpers

import           Control.Monad.Trans.Reader (runReaderT)
import qualified Network.Wai as NW
import qualified Servant as S
import           Servant ((:~>))

restHandlerToSHandler :: ConnectionPool -> RestHandler :~> S.Handler
restHandlerToSHandler pool = S.NT (`runReaderT` pool)

app :: ConnectionPool -> NW.Application
app pool = S.serve categoryApi $ S.enter (restHandlerToSHandler pool) categoryServer
