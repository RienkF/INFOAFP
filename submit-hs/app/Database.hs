{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.Migrations.InitMigration (initialSetupStep)

import GHC.Int
import Database.Beam.Migrate.Simple
import Database.Db
import Database.SQLite.Simple
import qualified Database.Beam.Sqlite.Migrate as Sqlite
import Database.Beam.Migrate
import Database.Model (User)
import Control.Arrow

submitDb :: DatabaseSettings Sqlite SubmitDb
submitDb = unCheckDatabase $ evaluateDatabase fullMigration

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks
  { runIrreversibleHook = pure True }

-- Chain the full migration here.
fullMigration :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite SubmitDb)
fullMigration = initialSetupStep

migrateDB :: Connection
          -> IO (Maybe (CheckedDatabaseSettings Sqlite SubmitDb))
migrateDB conn = runBeamSqliteDebug putStrLn conn $
  bringUpToDateWithHooks
    allowDestructive
    Sqlite.migrationBackend
    fullMigration

getSubmit :: Connection -> IO [User]
getSubmit conn = do
  runBeamSqlite conn $ do
    runSelectReturningList $ select (all_ (_users submitDb))