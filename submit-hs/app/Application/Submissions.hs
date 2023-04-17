module Application.Submissions where

import Database.Model
import qualified Database.Submissions
import Api.Types.SubmissionTypes (AddSubmissionBody (userId, assignmentId))
import Servant
import qualified Application.Users as Database.Users
import qualified Database.Model as Database
import qualified Database.Assignments as Database
import qualified Database.Submissions as Database.Submission
import Api.Types.ClassroomTypes (AddClassroomParticipantBody(classroomId))
import Control.Monad.IO.Class
import qualified Database.Assignments

getSubmissions :: Maybe [Int] -> Handler [Submission]
getSubmissions idFilter = liftIO $ Database.Submissions.getSubmissions idFilter

addSubmission :: AddSubmissionBody -> Handler (Maybe Submission)
addSubmission body = do
  users <- Database.Users.getUsers $ Just [userId body]
  assignment <- liftIO $ Database.Assignments.getAssignments $ Just [assignmentId body]

  case users of
    [user] -> case assignment of 
      [assignment] -> liftIO $ Database.Submission.addSubmission user assignment
      _ -> return Nothing
    _ -> return Nothing

