{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types.UserTypes where

import GHC.Generics
import Database.Model
import Data.Aeson
import Data.Aeson.Types

data AddUserBody = AddUserBody
  { userName :: String,
    userType :: UserType
  }
  deriving (Generic, FromJSON)

instance FromJSON UserType where
  parseJSON :: Value -> Parser UserType
  parseJSON = withText "userType" $ \case
    "teacher" -> pure Teacher
    "student" -> pure Student
    "ta" -> pure TA
    other -> fail $ "Unknown value for MyEnum: " ++ show other