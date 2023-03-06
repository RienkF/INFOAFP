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

import Data.Int (Int)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

data TestT f = Test
  { _id :: Columnar f Int,
    _foo :: Columnar f Text,
    _bar :: Columnar f Int
  }
  deriving (Generic)

type Test = TestT Identity

type TestId = PrimaryKey TestT Identity

instance Beamable TestT

deriving instance Show Test

deriving instance Eq Test

instance Table TestT where
  data PrimaryKey TestT f = TestId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey :: TestT column -> PrimaryKey TestT column
  primaryKey = TestId . _id

newtype TesttDb f = TestDb {tests :: f (TableEntity TestT)}
  deriving (Generic, Database be)

testDb :: DatabaseSettings be TesttDb
testDb = defaultDbSettings

getTest :: IO ()
getTest = do
  conn <- open "database.db"
  users <- runSelectReturningList $ select (all_ (tests testDb))
  mapM_ (liftIO . print) users