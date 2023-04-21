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

getSubmissions :: Maybe [Int] -> Maybe [Int] -> Maybe [Int] -> Handler [Submission]
getSubmissions submissionIds userIds assignmentIds = liftIO $ Database.Submissions.getSubmissions submissionIds userIds assignmentIds

addSubmission :: AddSubmissionBody -> Handler (Maybe Submission)
addSubmission body = do
  assignment <- liftIO $ Database.Assignments.getAssignments (Just [assignmentId body]) Nothing

  case assignment of
    [assignment] -> do
      -- User should be in the same classroom as the assignment
      let (ClassroomId classroomId) = _assignmentClassroom assignment
      users <- Database.Users.getUsers (Just [userId body]) (Just [fromIntegral classroomId])

      case users of
        [user] -> do
          submissions <-
            liftIO $
              Database.Submission.getSubmissions Nothing (Just [fromIntegral $ _userId user]) (Just [fromIntegral $ _assignmentId assignment])

          -- Only create the submission if the user does not yet have a submission for this assignment
          case submissions of
            [] -> liftIO $ Database.Submission.addSubmission user assignment
            _ -> return Nothing
        _ -> return Nothing
    _ -> return Nothing

deleteSubmission :: Int -> Handler NoContent
deleteSubmission id = liftIO $ Database.Submission.deleteSubmission id >> return NoContent