module Database.Assignments where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (assignments), databaseConnection, submitDb)
import Database.Model

getAssignments :: IO [Assignment]
getAssignments = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (assignments submitDb))