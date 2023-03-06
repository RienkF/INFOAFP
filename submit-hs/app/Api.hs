{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant
import Servant.API

type Spec = "test" :> Get '[JSON] TestResponse

data TestResponse = Response
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON TestResponse

api :: Proxy Spec
api = Proxy

testResponse1 :: TestResponse
testResponse1 = Response "Isaac Newton" 372

server :: Server Spec
server = return testResponse1

application :: Application
application = serve api server