module Application.Classrooms where

import Api.Types.ClassroomTypes
import Database.Beam
import qualified Database.Classrooms
import Database.Model
import qualified Database.Model as Database
import qualified Database.Users
import Servant

getClassrooms :: Maybe [Int] -> Maybe [Int] -> Handler [Classroom]
getClassrooms classroomIds userIds = liftIO $ Database.Classrooms.getClassrooms classroomIds userIds

getClassroomParticipants :: Handler [ClassroomParticipant]
getClassroomParticipants = liftIO Database.Classrooms.getClassroomParticipants

addClassroom :: AddClassroomBody -> Handler (Maybe Classroom)
addClassroom body = do
  teacherMaybe <- liftIO $ Database.Users.getUsers (Just [teacherId body]) Nothing
  case teacherMaybe of
    -- Require the given user to be a teacher
    [teacher@(User _ Teacher _)] -> do
      classroom <- liftIO $ Database.Classrooms.addClassroom (classroomName body)

      case classroom of
        (Just classroomVal) -> do
          -- Ideally, we'd have some sort of warning if this second operation fails, but we'll leave that for the sake of simplicity
          liftIO $ Database.Classrooms.addClassroomParticipant teacher classroomVal
          return classroom
        Nothing -> return Nothing
    _ -> return Nothing

deleteClassroom :: Int -> Handler NoContent
deleteClassroom id = liftIO $ Database.Classrooms.deleteClassroom id >> return NoContent

addClassroomParticipant :: AddClassroomParticipantBody -> Handler (Maybe ClassroomParticipant)
addClassroomParticipant body = do
  classrooms <- liftIO $ Database.Classrooms.getClassrooms (Just [classroomId body]) Nothing
  case classrooms of
    [classroom] -> do
      users <- liftIO $ Database.Users.getUsers (Just [userId body]) Nothing
      case users of
        [user] ->
          liftIO $
            Database.Classrooms.addClassroomParticipant
              user
              classroom
        -- There should be exactly one user for a given Id
        _ -> return Nothing
    -- There should be exactly one classroom for a given Id
    _ -> return Nothing