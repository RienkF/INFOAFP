{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Api.Spec where

import Data.Aeson
import Data.String (IsString (fromString))
import Database.Model
import Servant

type Spec =
  "users" :> Get '[JSON] [User]
    -- :<|> "classRooms" :> Get '[JSON] [ClassRoom]
    -- :<|> "classRoomsParticipants" :> Get '[JSON] [ClassRoomParticipant]
    -- :<|> "assignments" :> Get '[JSON] [Assignment]
    -- :<|> "submissions" :> Get '[JSON] [Submission]
    -- :<|> "attempts" :> Get '[JSON] [Attempt]
    -- :<|> "gradings" :> Get '[JSON] [Grading]
    :<|> Raw -- This final raw is just so we can serve the index file of the frontend

instance ToJSON UserType where
  toJSON :: UserType -> Value
  toJSON Teacher = String $ fromString "teacher"
  toJSON Student = String $ fromString "student"
  toJSON TA = String $ fromString "ta"

instance ToJSON User
