module Database.Classrooms where

import Data.String
import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (classroomParticipants, classrooms, users), databaseConnection, submitDb)
import Database.Model

getClassrooms :: Maybe [Int] -> Maybe [Int] -> IO [Classroom]
getClassrooms classRoomFilter userIdFilter = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $
      select $ do
        classrooms <- all_ (classrooms submitDb)
        case classRoomFilter of
          Just filter -> guard_ $ _classroomId classrooms `in_` map fromIntegral filter
          Nothing -> guard_ $ val_ True
        case userIdFilter of
          Just filter -> do
            users <- all_ (users submitDb)
            classroomParticipants <- all_ (classroomParticipants submitDb)

            guard_
              ( primaryKey classrooms
                  ==. _classroomParticipantClassroom classroomParticipants
                  &&. _classroomParticipantUser classroomParticipants
                  ==. primaryKey users
                  &&. _userId users
                  `in_` map fromIntegral filter
              )
          Nothing -> guard_ $ val_ True
        pure classrooms

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
