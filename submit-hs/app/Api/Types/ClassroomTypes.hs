{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Types.ClassroomTypes where

import Data.Aeson.Key
import Data.Aeson.Types
import Data.String
import Database.Model
import GHC.Generics

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
