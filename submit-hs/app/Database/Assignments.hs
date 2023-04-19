module Database.Assignments where

import Data.Text
import Data.Time (LocalTime)
import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (assignments), classrooms, databaseConnection, submitDb)
import Database.Model

getAssignments :: Maybe [Int] -> Maybe [Int] -> IO [Assignment]
getAssignments assignmentFilter classroomsFilter = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $
      select $ do
        assignments <- all_ (assignments submitDb)
        case assignmentFilter of
          Just filter -> guard_ $ _assignmentId assignments `in_` Prelude.map fromIntegral filter
          Nothing -> guard_ $ val_ True
        case classroomsFilter of
          Just filter -> do
            classrooms <- all_ (classrooms submitDb)
            guard_ $
              _classroomId classrooms
                `in_` Prelude.map fromIntegral filter
                &&. _assignmentClassroom assignments
                ==. primaryKey classrooms
          Nothing -> guard_ $ val_ True
        return assignments

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