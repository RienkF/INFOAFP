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

getAssignments :: IO [Assignment]
getAssignments = Database.Assignments.getAssignments Nothing

addAssignment :: AddAssignmentBody -> Handler (Maybe Assignment)
addAssignment body = do
  classrooms <- liftIO $ Database.Classrooms.getClassrooms $ Just [assignmentClassroom body]

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
