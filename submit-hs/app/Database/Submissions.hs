module Database.Submissions where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (assignments, submissions, users), databaseConnection, submitDb)
import Database.Model

getSubmissions :: Maybe [Int] -> Maybe [Int] -> Maybe [Int] -> IO [Submission]
getSubmissions submissionIds userIds assignmentIds = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select $ do
      submissions <- all_ (submissions submitDb)

      guard_ $ case submissionIds of
        Just filter -> _submissionId submissions `in_` map fromIntegral filter
        Nothing -> val_ True

      case userIds of
        Just filter -> do
          users <- all_ (users submitDb)
          guard_ $
            _userId users
              `in_` Prelude.map fromIntegral filter
              &&. _submissionUser submissions
              ==. primaryKey users
        Nothing -> guard_ $ val_ True

      case assignmentIds of
        Just filter -> do
          assignments <- all_ (assignments submitDb)
          guard_ $
            _assignmentId assignments
              `in_` Prelude.map fromIntegral filter
              &&. _submissionAssignment submissions
              ==. primaryKey assignments
        Nothing -> guard_ $ val_ True

      return submissions

addSubmission :: User -> Assignment -> IO (Maybe Submission)
addSubmission user assignment = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (submissions submitDb) $
          insertExpressions [Submission default_ (val_ $ pk user) (val_ $ pk assignment)]
    -- This should be only 1
    return
      ( case result of
          [submission] -> Just submission
          _ -> Nothing
      )
