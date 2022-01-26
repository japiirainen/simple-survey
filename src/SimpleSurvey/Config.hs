{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module SimpleSurvey.Config
    ( Environment(..)
    , Config(..)
    , AppT(..)
    , App
    , setLogger
    , katipLogger
    , makePool
    ) where

import           Control.Concurrent                   (ThreadId)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Logger                 (MonadLogger (..),
                                                       MonadLoggerIO (..))
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)

import           Control.Monad.Metrics                (Metrics,
                                                       MonadMetrics (..))
import           Control.Monad.Reader                 (MonadIO (..),
                                                       MonadReader,
                                                       MonadTrans (lift),
                                                       ReaderT (ReaderT), asks)
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)
import           Katip                                (Katip, KatipT (KatipT),
                                                       LogEnv, Severity (InfoS),
                                                       runKatipT)
import           Servant                              (ServerError)
import           Servant.Auth.Server                  (JWTSettings)

import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           Control.Exception.Safe               (throwIO)
import           Network.Wai.Handler.Warp             (Port)
import           System.Environment                   (lookupEnv)

import qualified Data.ByteString.Char8                as BS
import qualified SimpleSurvey.Logger                  as Logger

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

data Config = Config
    { configPool        :: ConnectionPool
    , configEnv         :: Environment
    , configMetrics     :: Metrics
    , configEkgServer   :: ThreadId
    , configJWTSettings :: JWTSettings
    , configLogEnv      :: LogEnv
    , configPort        :: Port
    }

newtype AppT m a = AppT { runAppT :: ReaderT Config (ExceptT ServerError m) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader Config, MonadError ServerError, MonadIO)

type App = AppT IO

instance Monad m => MonadMetrics (AppT m) where
    getMetrics = asks configMetrics

instance MonadIO m => Katip (AppT m) where
    getLogEnv = asks configLogEnv
    localLogEnv = error "not implemented"

instance MonadIO m => MonadLogger (AppT m) where
    monadLoggerLog = Logger.adapt Logger.logMsg

instance MonadIO m => MonadLogger (Logger.KatipT m) where
    monadLoggerLog = Logger.adapt Logger.logMsg

instance Katip IO where
    getLogEnv = Logger.defaultLogEnv
    localLogEnv _ io = io

instance MonadIO m => MonadLoggerIO (KatipT m) where
    askLoggerIO = KatipT $ pure $ Logger.adapt Logger.logMsg

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
    Logger.logMsg "web" InfoS "todo: received some request"
    liftIO $ app req respond

makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Test env = runKatipT env $ createPostgresqlPool (connStr "") (envPool Test)
makePool Development env = runKatipT env $ createPostgresqlPool (connStr "") (envPool Development)
makePool Production env = do
    pool <- runMaybeT $ do
        let keys = [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
        lift $ runKatipT env $ createPostgresqlPool prodStr (envPool Production)
    case pool of
        Nothing -> throwIO (userError "Database configuration not present in environment.")
        Just p -> pure p

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 3

connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=testdb" <> sfx <> " user=admin password=pass port=5432"
