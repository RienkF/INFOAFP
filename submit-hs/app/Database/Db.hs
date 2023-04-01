{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Db where

import Database.Beam
import Database.Model
import Database.Beam.Migrate
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite.Migrate

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

submitDb :: DatabaseSettings Sqlite SubmitDb
submitDb = unCheckDatabase checkedSubmitDb

getSubmit :: Connection -> IO [User]
getSubmit conn = do
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (_users submitDb))

checkedSubmitDb :: CheckedDatabaseSettings Sqlite SubmitDb
checkedSubmitDb = defaultMigratableDbSettings

migrateDb :: IO ()
migrateDb = do
  conn <- liftIO $ open "database.db"
  liftIO $ runBeamSqliteDebug putStrLn conn migrate
  where migrate = autoMigrate migrationBackend checkedSubmitDb