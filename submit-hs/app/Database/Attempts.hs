module Database.Attempts where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (attempts), databaseConnection, submitDb)
import Database.Model

getAttempts :: IO [Attempt]
getAttempts = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (attempts submitDb))