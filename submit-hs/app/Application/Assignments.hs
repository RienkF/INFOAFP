module Application.Assignments where

import Api.Types.AssignmentTypes
import Api.Types.ClassroomTypes
import Data.Maybe
import qualified Database.Assignments
import Database.Beam
import qualified Database.Classrooms
import Database.Model
import qualified Database.Model as Database
import qualified Database.Users
import Servant

getAssignments :: Maybe [Int] -> Maybe [Int] -> Handler [Assignment]
getAssignments assignmentIds classroomIds = liftIO $ Database.Assignments.getAssignments assignmentIds classroomIds

addAssignment :: AddAssignmentBody -> Handler (Maybe Assignment)
addAssignment body = do
  classrooms <- liftIO $ Database.Classrooms.getClassrooms (Just [assignmentClassroom body]) Nothing

  case classrooms of
    [classroom] ->
      liftIO $
        Database.Assignments.addAssignment
          (assignmentStartDate body)
          (assignmentDeadline body)
          (assignmentDescription body)
          (assignmentWeight body)
          classroom
    _ -> return Nothing

deleteAssignment :: Maybe Int -> Handler ()
deleteAssignment mId = case mId of
  Just id -> liftIO $ Database.Assignments.deleteAssignment id
  _ -> return ()
