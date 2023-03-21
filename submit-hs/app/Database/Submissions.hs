module Database.Submissions where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (submissions), databaseConnection, submitDb)
import Database.Model

getSubmissions :: IO [Submission]
getSubmissions = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (submissions submitDb))