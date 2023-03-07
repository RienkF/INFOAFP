{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.IO.Class
import Data.Aeson
import Data.String
import Database
import GHC.Generics
import Servant
import Servant.API

type Spec = "test" :> Get '[JSON] [TestResponse]

data TestResponse = Response
  { id :: Int,
    foo :: String,
    bar :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON TestResponse

api :: Proxy Spec
api = Proxy

server :: Server Spec
server = return $ liftIO $ do
  tests <- getTests
  Prelude.map (\test -> Response (_id test) (_foo test) (_bar test)) tests

application :: Application
application = serve api server