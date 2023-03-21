module Database.Gradings where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (gradings), databaseConnection, submitDb)
import Database.Model

getGradings :: IO [Grading]
getGradings = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (gradings submitDb))