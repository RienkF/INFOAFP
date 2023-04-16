module Application.Assignments where

import Database.Beam
import qualified Database.Classrooms
import Database.Model
import qualified Database.Model as Database
import qualified Database.Users
import Servant
import Api.Types.ClassroomTypes
import Api.Types.AssignmentTypes
import qualified Database.Assignments
import Debug.Trace
import Data.Maybe

getAssignments :: IO [Assignment]
getAssignments = Database.Assignments.getAssignments Nothing

addAssignment :: AddAssignmentBody -> Handler (Maybe Assignment)
addAssignment body = do
  classrooms <- liftIO $ Database.Classrooms.getClassrooms $ Just [assignmentClassroom body]

  t <- case classrooms of
    [classroom] -> liftIO $ Database.Assignments.addAssignment
        (assignmentStartDate body)
        (assignmentDeadline body)
        (assignmentDescription body)
        (assignmentWeight body)
        classroom
    _ -> return Nothing

  trace ("id: " ++ show (_assignmentId $ fromJust t)) $ return t