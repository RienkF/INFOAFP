{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Model where

import Data.Int
import Data.Text
import Data.Time
import Database.Beam
    ( Generic, Identity, Beamable, C, Columnar, Table(..) )
import qualified GHC.Int
import Database.Beam.Migrate (HasDefaultSqlDataType, BeamMigrateSqlBackend)
import Database.Beam.Backend
import Database.Beam.Migrate.Generics
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax, SqliteDataTypeSyntax, sqliteTextType)
import Database.Beam.Query.DataTypes

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

data ClassRoomT f = ClassRoom
  { _classRoomId :: C f Int32,
    _name :: C f Text
  }
  deriving (Generic, Beamable)

type ClassRoom = ClassRoomT Identity

instance Table ClassRoomT where
  data PrimaryKey ClassRoomT f = ClassRoomId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: ClassRoomT column -> PrimaryKey ClassRoomT column
  primaryKey = ClassRoomId . _classRoomId

-- Classroom Participants

data ClassRoomParticipantT f = ClassRoomParticipant
  { _participantClassRoom :: PrimaryKey ClassRoomT f,
    _participantUser :: PrimaryKey UserT f
  }
  deriving (Generic, Beamable)

type ClassRoomParticipant = ClassRoomParticipantT Identity

instance Table ClassRoomParticipantT where
  data PrimaryKey ClassRoomParticipantT f = ClassRoomParticipantId (PrimaryKey ClassRoomT f) (PrimaryKey UserT f)
    deriving (Generic, Beamable)
  primaryKey :: ClassRoomParticipantT column -> PrimaryKey ClassRoomParticipantT column
  primaryKey = ClassRoomParticipantId <$> _participantClassRoom <*> _participantUser

-- Assignment

data AssignmentT f = Assignment
  { _assignmentId :: C f Int32,
    _startDate :: C f LocalTime,
    _deadLine :: C f LocalTime,
    _description :: C f Text,
    _weight :: C f Double,
    _assignmentClassRoom :: PrimaryKey ClassRoomT f
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
    _file :: C f Text,
    _attemptTimeStamp :: C f LocalTime,
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
    _grade :: C f Double,
    -- User that did the review
    _gradingUser :: PrimaryKey UserT f,
    _gradingTimeStamp :: C f LocalTime,
    _feedback :: C f Text
  }
  deriving (Generic, Beamable)

type Grading = GradingT Identity

instance Table GradingT where
  data PrimaryKey GradingT f = GradingId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: GradingT column -> PrimaryKey GradingT column
  primaryKey = GradingId . _gradingId