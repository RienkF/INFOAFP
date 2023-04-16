module Application.Classrooms where

import Api.Types
import Database.Beam
import qualified Database.Classrooms
import Database.Model
import qualified Database.Model as Database
import qualified Database.Users
import Servant

getClassrooms :: IO [Classroom]
getClassrooms = Database.Classrooms.getClassrooms

getClassroomParticipants :: IO [ClassroomParticipant]
getClassroomParticipants = Database.Classrooms.getClassroomParticipants

addClassroom :: AddClassroomBody -> Handler (Maybe Classroom)
addClassroom body = do
  teacherMaybe <- liftIO $ Database.Users.getUsers $ Just [teacherId body]
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