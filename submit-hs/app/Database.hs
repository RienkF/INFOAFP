{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import GHC.Int

data TestT f = Test
  { _id :: Columnar f Int32,
    _foo :: Columnar f Text,
    _bar :: Columnar f Int32
  }
  deriving (Generic)

type Test = TestT Identity

type TestId = PrimaryKey TestT Identity

instance Beamable TestT

deriving instance Show Test

deriving instance Eq Test

instance Table TestT where
  data PrimaryKey TestT f = TestId (Columnar f GHC.Int.Int32) deriving (Generic, Beamable)
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