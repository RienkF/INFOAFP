{-# LANGUAGE OverloadedStrings #-}

module Database.Migrations.InitMigration (initialSetupStep) where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Sqlite
import Database.Db
import Database.Model

initialSetup :: Migration Sqlite (CheckedDatabaseSettings Sqlite SubmitDb)
initialSetup =
  SubmitDb
    <$> ( createTable "users" $
            User
              { _userId =
                  field
                    "id"
                    int
                    notNull
                    unique,
                _userType = field "type" userTypeProxy,
                _userName =
                  field
                    "name"
                    (nationalVarchar (Just 64))
              }
        )
    <*> ( createTable "classrooms" $
            Classroom
              { _classroomId =
                  field
                    "id"
                    int
                    notNull
                    unique,
                _classroomName =
                  field
                    "name"
                    (nationalVarchar (Just 64))
              }
        )
    <*> ( createTable "participants" $
            ClassroomParticipant
              { _classroomParticipantClassroom =
                  ClassroomId $
                    field "participant_classroom__id" int notNull,
                _classroomParticipantUser =
                  UserId $
                    field "participant_user__id" int notNull
              }
        )
    <*> ( createTable "assignments" $
            Assignment
              { _assignmentId =
                  field
                    "id"
                    int
                    notNull
                    unique,
                _assignmentStartDate =
                  field "start_date" timestamp,
                _assignmentDeadline =
                  field "deadline" timestamp,
                _assignmentDescription =
                  field
                    "description"
                    characterLargeObject
                    notNull,
                _assignmentWeight =
                  field
                    "weight"
                    double
                    notNull,
                _assignmentClassroom =
                  ClassroomId $
                    field "classroom__id" int notNull
              }
        )
    <*> ( createTable "submissions" $
            Submission
              { _submissionId =
                  field
                    "id"
                    int
                    notNull
                    unique,
                _submissionUser =
                  UserId $
                    field "user__id" int notNull,
                _submissionAssignment =
                  AssignmentId $
                    field "assignment__id" int notNull
              }
        )
    <*> ( createTable "attempts" $
            Attempt
              { _attemptId =
                  field
                    "id"
                    int
                    notNull
                    unique,
                _attemptFile =
                  field
                    "file"
                    characterLargeObject
                    notNull,
                _attemptTimestamp =
                  field "timestamp" timestamp,
                _attemptSubmission =
                  SubmissionId $
                    field "submission__id" int notNull
              }
        )
    <*> ( createTable "gradings" $
            Grading
              { _gradingId =
                  field
                    "id"
                    int
                    notNull
                    unique,
                _gradingSubmission =
                  SubmissionId $
                    field "submission__id" int notNull,
                _gradingGrade =
                  field "grade" double notNull,
                _gradingUser =
                  UserId $
                    field "user__id" int notNull,
                _gradingTimestamp =
                  field "timestamp" timestamp,
                _gradingFeedback =
                  field "feedback" characterLargeObject
              }
        )

initialSetupStep :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite SubmitDb)
initialSetupStep =
  migrationStep
    "initial_setup"
    (const initialSetup)
