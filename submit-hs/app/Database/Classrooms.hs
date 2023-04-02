module Database.Classrooms where

import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (classroomParticipants, classrooms), databaseConnection, submitDb)
import Database.Model

getClassrooms :: IO [Classroom]
getClassrooms = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (classrooms submitDb))

getClassroomParticipants :: IO [ClassroomParticipant]
getClassroomParticipants = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (classroomParticipants submitDb))
