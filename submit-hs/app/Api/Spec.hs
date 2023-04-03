{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Api.Spec where

import Api.Types
import Data.Aeson
import Data.Functor.Identity (Identity)
import Data.String (IsString (fromString))
import Database.Model
import Servant

type Spec =
  "users" :> (Get '[JSON] [User] :<|> "add" :> ReqBody '[JSON] AddUserBody :> PostNoContent)
    :<|> "classrooms" :> Get '[JSON] [Classroom]
    :<|> "classroomParticipants" :> Get '[JSON] [ClassroomParticipant]
    :<|> "assignments" :> Get '[JSON] [Assignment]
    :<|> "submissions" :> Get '[JSON] [Submission]
    :<|> "attempts" :> Get '[JSON] [Attempt]
    :<|> "gradings" :> Get '[JSON] [Grading]
    :<|> Raw -- This final raw is just so we can serve the index file of the frontend

instance ToJSON UserType where
  toJSON :: UserType -> Value
  toJSON Teacher = String $ fromString "teacher"
  toJSON Student = String $ fromString "student"
  toJSON TA = String $ fromString "ta"

instance (ToJSON (a f)) => ToJSON (PrimaryKey a f) where
  toJSON :: PrimaryKey a f -> Value
  toJSON = toJSON

instance ToJSON User

instance ToJSON Classroom

instance ToJSON ClassroomParticipant

instance ToJSON Assignment

instance ToJSON Submission

instance ToJSON Attempt

instance ToJSON Grading