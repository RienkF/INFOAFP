module Database.Users where

import Data.String
import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (users), databaseConnection, submitDb)
import Database.Model

getUsers :: Maybe [Int] -> IO [User]
getUsers idFilter = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $
      select
        ( filter_
            ( \user -> case idFilter of
                Just filter -> _userId user `in_` map fromIntegral filter
                Nothing -> val_ True
            )
            $ all_ (users submitDb)
        )

addUser :: String -> UserType -> IO (Maybe User)
addUser userName userType = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (users submitDb) $
          insertExpressions [User default_ (val_ userType) (val_ $ fromString userName)]
    -- This should be only 1
    return
      ( case result of
          [user] -> Just user
          _ -> Nothing
      )
