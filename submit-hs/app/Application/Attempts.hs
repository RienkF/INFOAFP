module Application.Attempts where

import qualified Database.Submissions
import qualified Database.Attempts
import Database.Model
import Api.Types.AttemptTypes
import Servant
import Database.Beam

getAttempts :: Maybe [Int] -> Handler [Attempt]
getAttempts idFilter = liftIO $ Database.Attempts.getAttempts idFilter

addAttempt :: AddAttemptBody -> Handler (Maybe Attempt)
addAttempt body = do
  submissions <- liftIO $ Database.Submissions.getSubmissions $ Just [submissionId body]

  case submissions of
    [submission] ->
      liftIO $
        Database.Attempts.addAttempt
          submission
          (file body)
    _ -> return Nothing
