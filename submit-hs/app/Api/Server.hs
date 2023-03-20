module Api.Server where

import Api hiding (api, server)
import Api.Spec
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data
import Database.Users (getUsers)
import Servant

api :: Proxy Api.Spec.Spec
api = Proxy

server :: Server Api.Spec.Spec
server =
  liftIO getUsers
    :<|> serveDirectoryFileServer "static/"

application :: Application
application = serve api server