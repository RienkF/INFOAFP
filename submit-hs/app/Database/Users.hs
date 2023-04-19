module Database.Users where

import Data.String
import Database.Beam
import Database.Beam.Sqlite
import Database.Db (SubmitDb (classroomParticipants, classrooms, users), databaseConnection, submitDb)
import Database.Model

getUsers :: Maybe [Int] -> Maybe [Int] -> IO [User]
getUsers userIds classroomIds = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    runSelectReturningList $
      select $ do
        users <- all_ (users submitDb)
        guard_ $ case userIds of
          Just filter -> _userId users `in_` map fromIntegral filter
          Nothing -> val_ True

        case classroomIds of
          Just filter -> do
            classrooms <- all_ (classrooms submitDb)
            classroomParticipants <- all_ (classroomParticipants submitDb)

            guard_
              ( primaryKey users
                  ==. _classroomParticipantUser classroomParticipants
                  &&. _classroomParticipantClassroom classroomParticipants
                  ==. primaryKey classrooms
                  &&. _classroomId classrooms
                  `in_` map fromIntegral filter
              )
          Nothing -> guard_ $ val_ True

        return users

addUser :: String -> UserType -> IO (Maybe User)
addUser userName userType = do
  conn <- databaseConnection
  runBeamSqlite conn $ do
    result <-
      runInsertReturningList $
        insert (users submitDb) $
          insertExpressions [User default_ (val_ userType) (val_ $ fromString userName)]
    -- This should be only 1
    return
      ( case result of
          [user] -> Just user
          _ -> Nothing
      )
