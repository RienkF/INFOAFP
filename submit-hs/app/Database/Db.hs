{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Db where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import qualified Database.Beam.Sqlite.Migrate as Sqlite
import Database.Model
import Database.SQLite.Simple

data SubmitDb f = SubmitDb
  { users :: f (TableEntity UserT),
    classRooms :: f (TableEntity ClassRoomT),
    classRoomParticipants :: f (TableEntity ClassRoomParticipantT),
    assignments :: f (TableEntity AssignmentT),
    submissions :: f (TableEntity SubmissionT),
    attempts :: f (TableEntity AttemptT),
    gradings :: f (TableEntity GradingT)
  }
  deriving (Generic, Database be)

submitDb :: DatabaseSettings be SubmitDb
submitDb = defaultDbSettings

migrateSubmitDb :: CheckedDatabaseSettings be SubmitDb
migrateSubmitDb = defaultMigratableDbSettings

databaseConnection :: IO Connection
databaseConnection = open "database.db"
