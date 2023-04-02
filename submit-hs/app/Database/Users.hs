module Database.Users where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (users), databaseConnection, submitDb)
import Database.Model

getUsers :: IO [User]
getUsers = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (users submitDb))