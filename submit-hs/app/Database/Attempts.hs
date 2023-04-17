module Database.Attempts where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (attempts), databaseConnection, submitDb)
import Database.Model
import Data.Text

getAttempts :: Maybe [Int] -> IO [Attempt]
getAttempts idFilter = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ 
      select 
        ( filter_
            ( \attempt -> case idFilter of
                Just filter -> _attemptId attempt `in_` Prelude.map fromIntegral filter
                Nothing -> val_ True
            )
            $ all_ (attempts submitDb)
        )

addAttempt :: Submission -> Text -> IO (Maybe Attempt)
addAttempt submission file = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (attempts submitDb) $
          insertExpressions [Attempt default_ (val_ file) currentTimestamp_ (val_ $ pk submission) ]
    -- This should be only 1
    return
      ( case result of
          [attempt] -> Just attempt
          _ -> Nothing
      )
