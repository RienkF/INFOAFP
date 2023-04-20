module Database.Gradings where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (gradings, submissions), databaseConnection, submitDb)
import Database.Model

getGradings :: Maybe [Int] -> Maybe [Int] -> IO [Grading]
getGradings gradingIds submissionIds = do
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

      return gradings