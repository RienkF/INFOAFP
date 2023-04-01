{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Model where

import Data.Int
import Data.Text
import Data.Time
import Database.Beam
  ( Beamable,
    C,
    Columnar,
    Generic,
    Identity,
    Table (..),
  )
import Database.Beam.Backend
import Database.Beam.Migrate (BeamMigrateSqlBackend, HasDefaultSqlDataType)
import Database.Beam.Migrate.Generics
import Database.Beam.Query.DataTypes
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Syntax (SqliteDataTypeSyntax, SqliteValueSyntax, sqliteTextType)
import qualified GHC.Int

data UserType = Teacher | TA | Student
  deriving (Show, Read, Eq, Ord, Enum)

-- Beam has to know what the default value should be for UserType column in the db
instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be UserType where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance (BeamBackend Sqlite, FromBackendRow Sqlite Text) => FromBackendRow Sqlite UserType where
  fromBackendRow :: FromBackendRowM Sqlite UserType
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text of
      "teacher" -> pure Teacher
      "ta" -> pure TA
      "student" -> pure Student
      _ -> fail "Invalid"

userTypeProxy :: DataType Sqlite UserType
userTypeProxy = DataType sqliteTextType

data UserT f = User
  { _userId :: C f Int32,
    _userType :: C f UserType,
    _userName :: C f Text
  }
  deriving (Generic, Beamable)

type User = UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: UserT column -> PrimaryKey UserT column
  primaryKey = UserId . _userId

-- Classroom

data ClassroomT f = Classroom
  { _classroomId :: C f Int32,
    _classroomName :: C f Text
  }
  deriving (Generic, Beamable)

type Classroom = ClassroomT Identity

instance Table ClassroomT where
  data PrimaryKey ClassroomT f = ClassroomId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: ClassroomT column -> PrimaryKey ClassroomT column
  primaryKey = ClassroomId . _classroomId

-- Classroom Participants

data ClassroomParticipantT f = ClassroomParticipant
  { _classroomParticipantClassroom :: PrimaryKey ClassroomT f,
    _classroomParticipantUser :: PrimaryKey UserT f
  }
  deriving (Generic, Beamable)

type ClassroomParticipant = ClassroomParticipantT Identity

instance Table ClassroomParticipantT where
  data PrimaryKey ClassroomParticipantT f = ClassroomParticipantId (PrimaryKey ClassroomT f) (PrimaryKey UserT f)
    deriving (Generic, Beamable)
  primaryKey :: ClassroomParticipantT column -> PrimaryKey ClassroomParticipantT column
  primaryKey = ClassroomParticipantId <$> _classroomParticipantClassroom <*> _classroomParticipantUser

-- Assignment

data AssignmentT f = Assignment
  { _assignmentId :: C f Int32,
    _assignmentStartDate :: C f LocalTime,
    _assignmentDeadline :: C f LocalTime,
    _assignmentDescription :: C f Text,
    _assignmentWeight :: C f Double,
    _assignmentClassroom :: PrimaryKey ClassroomT f
  }
  deriving (Generic, Beamable)

type Assignment = AssignmentT Identity

instance Table AssignmentT where
  data PrimaryKey AssignmentT f = AssignmentId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: AssignmentT column -> PrimaryKey AssignmentT column
  primaryKey = AssignmentId . _assignmentId

-- Submission

data SubmissionT f = Submission
  { _submissionId :: C f Int32,
    _submissionUser :: PrimaryKey UserT f,
    _submissionAssignment :: PrimaryKey AssignmentT f
  }
  deriving (Generic, Beamable)

type Submission = SubmissionT Identity

instance Table SubmissionT where
  data PrimaryKey SubmissionT f = SubmissionId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: SubmissionT column -> PrimaryKey SubmissionT column
  primaryKey = SubmissionId . _submissionId

-- Attempt

data AttemptT f = Attempt
  { _attemptId :: C f Int32,
    _attemptFile :: C f Text,
    _attemptTimestamp :: C f LocalTime,
    _attemptSubmission :: PrimaryKey SubmissionT f
  }
  deriving (Generic, Beamable)

type Attempt = AttemptT Identity

instance Table AttemptT where
  data PrimaryKey AttemptT f = AttemptId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: AttemptT column -> PrimaryKey AttemptT column
  primaryKey = AttemptId . _attemptId

-- Grading

data GradingT f = Grading
  { _gradingId :: C f Int32,
    _gradingSubmission :: PrimaryKey SubmissionT f,
    _gradingGrade :: C f Double,
    -- User that did the review
    _gradingUser :: PrimaryKey UserT f,
    _gradingTimestamp :: C f LocalTime,
    _gradingFeedback :: C f Text
  }
  deriving (Generic, Beamable)

type Grading = GradingT Identity

instance Table GradingT where
  data PrimaryKey GradingT f = GradingId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: GradingT column -> PrimaryKey GradingT column
  primaryKey = GradingId . _gradingId