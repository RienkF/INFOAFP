module Database.Assignments where

import Data.Text
import Data.Time (LocalTime)
import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (assignments), classrooms, databaseConnection, submitDb)
import Database.Model

getAssignments :: Maybe [Int] -> IO [Assignment]
getAssignments assignmentFilter = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $
      select
        ( filter_
            ( \assignment -> case assignmentFilter of
                Just filter -> _assignmentId assignment `in_` Prelude.map fromIntegral filter
                Nothing -> val_ True
            )
            $ all_ (assignments submitDb)
        )

addAssignment :: LocalTime -> LocalTime -> Text -> Double -> Classroom -> IO (Maybe Assignment)
addAssignment startDate deadline description weight classroom = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (assignments submitDb) $
          insertExpressions
            [ Assignment
                default_
                (val_ startDate)
                (val_ deadline)
                (val_ description)
                (val_ weight)
                (val_ $ pk classroom)
            ]
    -- This should be only 1
    return
      ( case result of
          [assignment] -> Just assignment
          _ -> Nothing
      )