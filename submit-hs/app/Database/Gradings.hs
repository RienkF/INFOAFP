module Database.Gradings where

import Data.Text
import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (assignments, gradings, submissions), databaseConnection, submitDb)
import Database.Model
import Servant

getGradings :: Maybe [Int] -> Maybe [Int] -> Maybe [Int] -> IO [Grading]
getGradings gradingIds submissionIds assignmentIds = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select $ do
      gradings <- all_ $ gradings submitDb

      guard_ $
        case gradingIds of
          Just filter -> _gradingId gradings `in_` Prelude.map fromIntegral filter
          Nothing -> val_ True

      case submissionIds of
        Just filter -> do
          submissions <- all_ (submissions submitDb)
          guard_ $
            _submissionId submissions
              `in_` Prelude.map fromIntegral filter
              &&. _gradingSubmission gradings
              ==. primaryKey submissions
        Nothing -> guard_ $ val_ True

      case assignmentIds of
        Just filter -> do
          assignments <- all_ (assignments submitDb)
          submissions <- all_ (submissions submitDb)
          guard_ $
            _assignmentId assignments
              `in_` Prelude.map fromIntegral filter
              &&. _submissionAssignment submissions
              ==. primaryKey assignments
              &&. primaryKey submissions
              ==. _gradingSubmission gradings
        Nothing -> guard_ $ val_ True

      return gradings

addGrading :: Submission -> Double -> User -> Text -> IO (Maybe Grading)
addGrading submission grade reviewer feedback = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (gradings submitDb) $
          insertExpressions [Grading default_ (val_ $ pk submission) (val_ grade) (val_ $ pk reviewer) currentTimestamp_ (val_ feedback)]
    -- This has to  return a single Grading
    return
      ( case result of
          [grading] -> Just grading
          _ -> Nothing
      )

deleteGrading :: Int -> IO ()
deleteGrading id = do
  conn <- databaseConnection
  runBeamSqlite conn $ runDelete $ delete (gradings submitDb)
        (\c -> _gradingId c ==. fromIntegral id)