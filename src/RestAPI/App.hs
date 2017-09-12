{-# LANGUAGE TypeOperators #-}

module RestAPI.App where

import           RestAPI.Category.Endpoints
import           RestAPI.RestHelpers

import           Control.Monad.Trans.Reader (runReaderT)
import qualified Network.Wai as NW
import qualified Servant as S
import           Servant ((:~>))

restHandlerToSHandler :: ReaderItems -> RestHandler :~> S.Handler
restHandlerToSHandler readerItems = S.NT (`runReaderT` readerItems)

app :: ReaderItems -> NW.Application
app readerItems = S.serve categoryApi $ S.enter (restHandlerToSHandler readerItems)
                                                categoryServer
