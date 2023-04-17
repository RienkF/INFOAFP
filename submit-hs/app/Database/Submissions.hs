module Database.Submissions where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (submissions, assignments), databaseConnection, submitDb)
import Database.Model

getSubmissions :: Maybe [Int] -> IO [Submission]
getSubmissions idFilter = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ 
      select 
        ( filter_
          ( \submission -> case idFilter of 
              Just filter -> _submissionId submission `in_` Prelude.map fromIntegral filter
              Nothing -> val_ True
          )
          $ all_ (submissions submitDb)
        )
          

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
