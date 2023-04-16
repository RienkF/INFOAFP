{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Types where

import Data.Aeson.Key
import Data.Aeson.Types
import Data.String
import Database.Model
import GHC.Generics

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

data AddClassroomBody = AddClassroomBody
  { teacherId :: Int,
    classroomName :: String
  }
  deriving (Generic, FromJSON)

data AddClassroomParticipantBody = AddClassroomParticipantBody
  { userId :: Int,
    classroomId :: Int
  }
  deriving (Generic, FromJSON)
