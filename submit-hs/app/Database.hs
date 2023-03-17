{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: Remove this when api has been updated, this is all testing stuff but not actually needed

module Database where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite

import Database.Migrations.InitMigration (initialSetupStep)

import GHC.Int
import Database.Beam.Migrate.Simple
    ( CheckedDatabaseSettings,
      bringUpToDateWithHooks,
      defaultUpToDateHooks,
      BringUpToDateHooks(runIrreversibleHook), MigrationSteps )
import Database.Db (SubmitDb)
import Database.SQLite.Simple
import qualified Database.Beam.Sqlite.Migrate as Sqlite
import Database.Beam.Migrate
import Database.Model (User)

fullMigration :: MigrationSteps Sqlite
  ()
  (CheckedDatabaseSettings Sqlite SubmitDb)
fullMigration = initialSetupStep

submitDb :: DatabaseSettings Sqlite SubmitDb
submitDb = unCheckDatabase $ evaluateDatabase fullMigration

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks
  { runIrreversibleHook = pure True }

getSubmit :: IO [User]
getSubmit = do
  conn <- open "database.db"
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (test submitDb))