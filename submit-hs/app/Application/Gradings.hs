module Application.Gradings where

import qualified Database.Gradings
import Database.Model

getGradings :: IO [Grading]
getGradings = Database.Gradings.getGradings