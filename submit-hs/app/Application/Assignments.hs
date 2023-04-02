module Application.Assignments where

import qualified Database.Assignments
import Database.Model
import qualified Database.Model as Database

getAssignments :: IO [Assignment]
getAssignments = Database.Assignments.getAssignments