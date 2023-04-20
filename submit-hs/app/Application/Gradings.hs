module Application.Gradings where

import Api.Types.GradingTypes
import Database.Beam
import qualified Database.Gradings
import Database.Model
import Database.Submissions
import Database.Users
import Servant

getGradings :: Maybe [Int] -> Maybe [Int] -> Maybe [Int] -> Handler [Grading]
getGradings gradingIds submissionIds assignmentIds =
  liftIO $
    Database.Gradings.getGradings gradingIds submissionIds assignmentIds

addGrading :: AddGradingBody -> Handler (Maybe Grading)
addGrading body = do
  submissions <- liftIO $ getSubmissions (Just [submissionId body]) Nothing Nothing

  -- there should be only a single submission associated
  case submissions of
    [submission] -> do
      reviewers <- liftIO $ getUsers (Just [reviewerId body]) Nothing
      -- there should be only a single reviewer associated
      case reviewers of
        [reviewer] ->
          liftIO $ Database.Gradings.addGrading submission (grade body) reviewer (feedback body)
        _ -> return Nothing
    _ -> return Nothing
