{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module SimpleSurvey.Database where

import           Control.Monad.Reader        (MonadIO, MonadReader, asks,
                                              liftIO)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (BackendKey (SqlBackendKey),
                                              SqlPersistT, runMigration,
                                              runSqlPool)
import           Database.Persist.Quasi      (lowerCaseSettings)
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistManyFileWith, share,
                                              sqlSettings)
import           Say                         (say)

import           Data.Password.Bcrypt        (Bcrypt, PasswordHash (..))
import           Data.Password.Instances     ()

import           SimpleSurvey.Config         (Config, configPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistManyFileWith lowerCaseSettings [
      "config/models/User.persistentmodels"
    ]
  )

doMigrations :: SqlPersistT IO ()
doMigrations = do
    liftIO $ say "Database.doMigrations"
    runMigration migrateAll
    liftIO $ say "Migrations ran successfully"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
