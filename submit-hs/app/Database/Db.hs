{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Db where

import Database.Beam
import Database.Beam.Migrate
  ( CheckedDatabaseSettings,
    defaultMigratableDbSettings,
    unCheckDatabase,
  )
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.Model
import Database.SQLite.Simple

data SubmitDb f = SubmitDb
  { users :: f (TableEntity UserT),
    classrooms :: f (TableEntity ClassroomT),
    classroomParticipants :: f (TableEntity ClassroomParticipantT),
    assignments :: f (TableEntity AssignmentT),
    submissions :: f (TableEntity SubmissionT),
    attempts :: f (TableEntity AttemptT),
    gradings :: f (TableEntity GradingT)
  }
  deriving (Generic, Database be)

submitDb :: DatabaseSettings Sqlite SubmitDb
submitDb = unCheckDatabase checkedSubmitDb

getSubmit :: Connection -> IO [User]
getSubmit conn = do
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (users submitDb))

checkedSubmitDb :: CheckedDatabaseSettings Sqlite SubmitDb
checkedSubmitDb = defaultMigratableDbSettings

migrateDb :: IO ()
migrateDb = do
  conn <- liftIO $ open "database.db"
  liftIO $ runBeamSqliteDebug putStrLn conn migrate
  where
    migrate = autoMigrate migrationBackend checkedSubmitDb

databaseConnection :: IO Connection
databaseConnection = open "database.db"