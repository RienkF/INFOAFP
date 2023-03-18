{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Monad.IO.Class
import Data.Aeson
import Data.String
import Data.Text
import GHC.Generics
import GHC.Int
import Servant
import Servant.API
import Database
import Prelude hiding (id)
import Database.SQLite.Simple

type Spec = "test" :> Get '[JSON] [TestResponse] :<|> Raw

data TestResponse = Response
  { id :: Int32,
    foo :: Text,
    bar :: Int32
  }
  deriving (Eq, Show, Generic)

instance ToJSON TestResponse

api :: Proxy Spec
api = Proxy

server :: Server Spec
server =
  ( do
      tests <- liftIO $ open "database.db" >>= getSubmit
      return [Response { id=1, foo="", bar=2}]
  )
    :<|> serveDirectoryFileServer "static/"

application :: Application
application = serve api server