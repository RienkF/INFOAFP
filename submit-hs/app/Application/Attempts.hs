module Application.Attempts where

import Api.Types.AttemptTypes
import qualified Database.Attempts
import Database.Beam
import Database.Model
import qualified Database.Submissions
import Servant

getAttempts :: Maybe [Int] -> Maybe [Int] -> Handler [Attempt]
getAttempts attemptIds submissionIds = liftIO $ Database.Attempts.getAttempts attemptIds submissionIds

addAttempt :: AddAttemptBody -> Handler (Maybe Attempt)
addAttempt body = do
  submissions <- liftIO $ Database.Submissions.getSubmissions (Just [submissionId body]) Nothing Nothing

  case submissions of
    [submission] ->
      liftIO $
        Database.Attempts.addAttempt
          submission
          (file body)
    _ -> return Nothing
