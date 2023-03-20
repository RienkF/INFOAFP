module Database.Migrations.Migrate where

import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import qualified Database.Beam.Sqlite.Migrate as Sqlite
import Database.Db
import Database.Migrations.InitMigration
import Database.SQLite.Simple

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = pure True
    }

-- Chain the full migration here.
fullMigration :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite SubmitDb)
fullMigration = initialSetupStep

-- migrateDB ::
--   Connection ->
--   IO (Maybe (CheckedDatabaseSettings Sqlite SubmitDb))
migrateDB conn =
  runBeamSqliteDebug putStrLn conn $
    -- bringUpToDateWithHooks
    --   allowDestructive
    --   Sqlite.migrationBackend
    --   fullMigration
    autoMigrate
      Sqlite.migrationBackend
      migrateSubmitDb

-- migrateDatabase :: IO (Maybe (CheckedDatabaseSettings Sqlite SubmitDb))
migrateDatabase = do
  conn <- databaseConnection
  migrateDB conn