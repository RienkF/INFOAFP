module Application.Classrooms where

import qualified Database.Classrooms
import Database.Model

getClassrooms :: IO [Classroom]
getClassrooms = Database.Classrooms.getClassrooms

getClassroomParticipants :: IO [ClassroomParticipant]
getClassroomParticipants = Database.Classrooms.getClassroomParticipants