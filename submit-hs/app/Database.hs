{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: Remove this when api has been updated, this is all testing stuff but not actually needed

module Database where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import GHC.Int

data TestT f = Test
  { _id :: C f Int32,
    _foo :: C f Text,
    _bar :: C f Int32
  }
  deriving (Generic, Beamable)

type Test = TestT Identity

type TestId = PrimaryKey TestT Identity

deriving instance Show Test

deriving instance Eq Test

instance Table TestT where
  data PrimaryKey TestT f = TestId (C f GHC.Int.Int32) deriving (Generic, Beamable)
  primaryKey :: TestT column -> PrimaryKey TestT column
  primaryKey = TestId . _id

newtype TesttDb f = TestDb {test :: f (TableEntity TestT)}
  deriving (Generic, Database be)

testDb :: DatabaseSettings be TesttDb
testDb = defaultDbSettings

getTest :: IO [Test]
getTest = do
  conn <- open "database.db"
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (test testDb))