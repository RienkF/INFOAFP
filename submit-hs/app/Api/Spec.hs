{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Api.Spec where

import Data.Aeson
import Data.Functor.Identity (Identity)
import Data.String (IsString (fromString))
import Database.Model
import Servant
import Api.Types.UserTypes
import Api.Types.ClassroomTypes
import Api.Types.SubmissionTypes (AddSubmissionBody)
import Api.Types.AssignmentTypes (AddAssignmentBody)

type Spec =
  "users"
    :> ( QueryParam "userIds" [Int] :> Get '[JSON] [User]
           :<|> "add" :> ReqBody '[JSON] AddUserBody :> Post '[JSON] (Maybe User)
       )
    :<|> "classrooms"
      :> ( Get '[JSON] [Classroom]
             :<|> "add" :> ReqBody '[JSON] AddClassroomBody :> Post '[JSON] (Maybe Classroom)
         )
    :<|> "classroomParticipants" :> Get '[JSON] [ClassroomParticipant]
    :<|> "assignments" 
      :> ( Get '[JSON] [Assignment]
             :<|> "add" :> ReqBody '[JSON] AddAssignmentBody :> Post '[JSON] (Maybe Assignment)
         )
    :<|> "submissions" 
      :> ( Get '[JSON] [Submission]
             :<|> "add" :> ReqBody '[JSON] AddSubmissionBody :> Post '[JSON] (Maybe Submission)
         )
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
