{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Db where

import Database.Beam
import Database.Model
import Database.Beam.Migrate
import Database.Beam.Sqlite

data SubmitDb f = SubmitDb
  { _users :: f (TableEntity UserT),
    _classRooms :: f (TableEntity ClassRoomT),
    _classRoomParticipants :: f (TableEntity ClassRoomParticipantT),
    _assignments :: f (TableEntity AssignmentT),
    _submissions :: f (TableEntity SubmissionT),
    _attempts :: f (TableEntity AttemptT),
    _gradings :: f (TableEntity GradingT)
  }
  deriving (Generic, Database be)