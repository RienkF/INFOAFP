module Application.Submissions where

import Api.Types.ClassroomTypes (AddClassroomParticipantBody (classroomId))
import Api.Types.SubmissionTypes (AddSubmissionBody (assignmentId, userId))
import qualified Application.Users as Database.Users
import Control.Monad.IO.Class
import qualified Database.Assignments
import qualified Database.Assignments as Database
import Database.Model
import qualified Database.Model as Database
import qualified Database.Submissions
import qualified Database.Submissions as Database.Submission
import Servant

getSubmissions :: IO [Submission]
getSubmissions = Database.Submissions.getSubmissions Nothing

addSubmission :: AddSubmissionBody -> Handler (Maybe Submission)
addSubmission body = do
  users <- Database.Users.getUsers (Just [userId body]) Nothing
  assignment <- liftIO $ Database.Assignments.getAssignments (Just [assignmentId body]) Nothing

  case users of
    [user] -> case assignment of
      [assignment] -> liftIO $ Database.Submission.addSubmission user assignment
      _ -> return Nothing
    _ -> return Nothing
