module Database.Classrooms where

import Data.String
import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (classroomParticipants, classrooms), databaseConnection, submitDb)
import Database.Model

getClassrooms :: Maybe [Int] -> IO [Classroom]
getClassrooms classRoomFilter = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select 
      ( filter_
        ( \classroom -> case classRoomFilter of
            Just filter -> _classroomId classroom `in_` map fromIntegral filter
            Nothing -> val_ True
        )
        $ all_ (classrooms submitDb)
      )

getClassroomParticipants :: IO [ClassroomParticipant]
getClassroomParticipants = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (classroomParticipants submitDb))

addClassroom :: String -> IO (Maybe Classroom)
addClassroom classroomName = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (classrooms submitDb) $
          insertExpressions [Classroom default_ (val_ $ fromString classroomName)]
    -- This should be only 1
    return
      ( case result of
          [classroom] -> Just classroom
          _ -> Nothing
      )

addClassroomParticipant :: User -> Classroom -> IO (Maybe ClassroomParticipant)
addClassroomParticipant user classroom = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (classroomParticipants submitDb) $
          insertValues [ClassroomParticipant (pk classroom) (pk user)]
    -- This should be only 1
    return
      ( case result of
          [classroomParticipant] -> Just classroomParticipant
          _ -> Nothing
      )
