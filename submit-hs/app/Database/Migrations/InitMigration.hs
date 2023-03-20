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
    <*> ( createTable "classRooms" $
            ClassRoom
              { _classRoomId =
                  field
                    "id"
                    int
                    notNull
                    unique,
                _name =
                  field
                    "name"
                    (nationalVarchar (Just 64))
              }
        )
    <*> ( createTable "classroom_participants" $
            ClassRoomParticipant
              { _participantClassRoom =
                  ClassRoomId $
                    field "participant_classroom" int notNull,
                _participantUser =
                  UserId $
                    field "participant_user" int notNull
              }
        )
    <*> ( createTable "assignments" $
            Assignment
              { _assignmentId =
                  field
                    "assignment_id"
                    int
                    notNull
                    unique,
                _startDate =
                  field "start_date" timestamp,
                _deadLine =
                  field "deadline" timestamp,
                _description =
                  field
                    "description"
                    characterLargeObject
                    notNull,
                _weight =
                  field
                    "weight"
                    double
                    notNull,
                _assignmentClassRoom =
                  ClassRoomId $
                    field "assignment_classroom" int notNull
              }
        )
    <*> ( createTable "submissions" $
            Submission
              { _submissionId =
                  field
                    "submission_id"
                    int
                    notNull
                    unique,
                _submissionUser =
                  UserId $
                    field "submission_user" int notNull,
                _submissionAssignment =
                  AssignmentId $
                    field "submission_assignment" int notNull
              }
        )
    <*> ( createTable "attempts" $
            Attempt
              { _attemptId =
                  field
                    "attempt_id"
                    int
                    notNull
                    unique,
                _file =
                  field
                    "file"
                    characterLargeObject
                    notNull,
                _attemptTimeStamp =
                  field "attempt_timestamp" timestamp,
                _attemptSubmission =
                  SubmissionId $
                    field "attempt_submission" int notNull
              }
        )
    <*> ( createTable "gradings" $
            Grading
              { _gradingId =
                  field
                    "grading_id"
                    int
                    notNull
                    unique,
                _gradingSubmission =
                  SubmissionId $
                    field "grading_submission" int notNull,
                _grade =
                  field "grade" double notNull,
                _gradingUser =
                  UserId $
                    field "grading_user" int notNull,
                _gradingTimeStamp =
                  field "grading_timestamp" timestamp,
                _feedback =
                  field "feedback" characterLargeObject
              }
        )

initialSetupStep :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite SubmitDb)
initialSetupStep =
  migrationStep
    "initial_setup"
    (const initialSetup)
