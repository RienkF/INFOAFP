module Application.Gradings where

import Database.Beam
import qualified Database.Gradings
import Database.Model
import Servant
import Database.Beam
import Api.Types.GradingTypes
import Database.Users
import Database.Submissions

getGradings :: Maybe [Int] -> Maybe [Int] -> Handler [Grading]
getGradings gradingIds submissionIds = liftIO $ Database.Gradings.getGradings gradingIds submissionIds

addGrading :: AddGradingBody -> Handler (Maybe Grading)
addGrading body = do
  submissions <- liftIO $ getSubmissions (Just [submissionId body]) Nothing Nothing

  -- there should be only a single submission associated
  case submissions of
    [ submission ] -> do
      reviewers <- liftIO $ getUsers (Just [reviewerId body]) Nothing
      -- there should be only a single reviewer associated
      case reviewers of
        [ reviewer ] -> undefined
        _ -> return Nothing
    _ -> return Nothing

