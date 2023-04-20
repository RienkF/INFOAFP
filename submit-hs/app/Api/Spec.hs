{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Api.Spec where

import Api.Types.AssignmentTypes (AddAssignmentBody)
import Api.Types.AttemptTypes
import Api.Types.ClassroomTypes
import Api.Types.GradingTypes
import Api.Types.SubmissionTypes (AddSubmissionBody)
import Api.Types.UserTypes
import Data.Aeson
import Data.Functor.Identity (Identity)
import Data.String (IsString (fromString))
import Database.Beam (SqlValable (val_))
import Database.Model
import Servant

type Spec =
  "users"
    :> ( QueryParam "userIds" [Int] :> QueryParam "classroomIds" [Int] :> Get '[JSON] [User]
           :<|> "add" :> ReqBody '[JSON] AddUserBody :> Post '[JSON] (Maybe User)
       )
    :<|> "classrooms"
      :> ( QueryParam "classroomIds" [Int] :> QueryParam "userIds" [Int] :> Get '[JSON] [Classroom]
             :<|> "add" :> ReqBody '[JSON] AddClassroomBody :> Post '[JSON] (Maybe Classroom)
         )
    :<|> "classroomParticipants"
      :> ( Get '[JSON] [ClassroomParticipant]
             :<|> "add" :> ReqBody '[JSON] AddClassroomParticipantBody :> Post '[JSON] (Maybe ClassroomParticipant)
         )
    :<|> "assignments"
      :> ( QueryParam "assignmentIds" [Int] :> QueryParam "classroomIds" [Int] :> Get '[JSON] [Assignment]
             :<|> "add" :> ReqBody '[JSON] AddAssignmentBody :> Post '[JSON] (Maybe Assignment)
         )
    :<|> "submissions"
      :> ( QueryParam "submissionIds" [Int] :> QueryParam "userIds" [Int] :> QueryParam "assignmentIds" [Int] :> Get '[JSON] [Submission]
             :<|> "add" :> ReqBody '[JSON] AddSubmissionBody :> Post '[JSON] (Maybe Submission)
         )
    :<|> "attempts"
      :> ( QueryParam "attemptIds" [Int] :> QueryParam "submissionIds" [Int] :> Get '[JSON] [Attempt]
             :<|> "add" :> ReqBody '[JSON] AddAttemptBody :> Post '[JSON] (Maybe Attempt)
         )
    :<|> "gradings"
      :> ( QueryParam "gradingIds" [Int] :> QueryParam "submissionIds" [Int] :> QueryParam "assignmentIds" [Int] :> Get '[JSON] [Grading]
             :<|> "add" :> ReqBody '[JSON] AddGradingBody :> Post '[JSON] (Maybe Grading)
         )
    :<|> Raw -- This final raw is just so we can serve the index file of the frontend

instance ToJSON UserType where
  toJSON :: UserType -> Value
  toJSON Teacher = String $ fromString "teacher"
  toJSON Student = String $ fromString "student"
  toJSON TA = String $ fromString "ta"

instance ToJSON User

instance ToJSON (PrimaryKey UserT Identity) where
  toJSON :: PrimaryKey UserT Identity -> Value
  toJSON (UserId id) = toJSON id

instance ToJSON Classroom

instance ToJSON (PrimaryKey ClassroomT Identity) where
  toJSON :: PrimaryKey ClassroomT Identity -> Value
  toJSON (ClassroomId id) = toJSON id

instance ToJSON ClassroomParticipant

instance ToJSON Assignment

instance ToJSON (PrimaryKey AssignmentT Identity) where
  toJSON :: PrimaryKey AssignmentT Identity -> Value
  toJSON (AssignmentId id) = toJSON id

instance ToJSON Submission

instance ToJSON (PrimaryKey SubmissionT Identity) where
  toJSON :: PrimaryKey SubmissionT Identity -> Value
  toJSON (SubmissionId id) = toJSON id

instance ToJSON Attempt

instance ToJSON Grading
