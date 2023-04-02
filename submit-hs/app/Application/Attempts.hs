module Application.Attempts where

import qualified Database.Attempts
import Database.Model

getAttempts :: IO [Attempt]
getAttempts = Database.Attempts.getAttempts