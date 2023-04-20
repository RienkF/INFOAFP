module Database.Attempts where

import Data.Text
import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (attempts, submissions), databaseConnection, submitDb)
import Database.Model

getAttempts :: Maybe [Int] -> Maybe [Int] -> IO [Attempt]
getAttempts attemptIds submissionIds = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $
      select $ do
        attempts <- all_ (attempts submitDb)

        guard_ $
          case attemptIds of
            Just filter -> _attemptId attempts `in_` Prelude.map fromIntegral filter
            Nothing -> val_ True

        case submissionIds of
          Just filter -> do
            submissions <- all_ (submissions submitDb)
            guard_ $
              _submissionId submissions
                `in_` Prelude.map fromIntegral filter
                &&. _attemptSubmission attempts
                ==. primaryKey submissions
          Nothing -> guard_ $ val_ True

        return attempts

addAttempt :: Submission -> Text -> IO (Maybe Attempt)
addAttempt submission file = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (attempts submitDb) $
          insertExpressions [Attempt default_ (val_ file) currentTimestamp_ (val_ $ pk submission)]
    -- This should be only 1
    return
      ( case result of
          [attempt] -> Just attempt
          _ -> Nothing
      )
