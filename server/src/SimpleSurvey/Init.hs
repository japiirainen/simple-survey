{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleSurvey.Init
  ( withConfig,
    runApp,
    shutDownApp,
    initialize,
  )
where

import Control.Concurrent (killThread)
import Control.Exception
import qualified Control.Monad.Metrics as M
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (typeOf)
import Database.Persist.Postgresql (runSqlPool)
import qualified Katip
import Lens.Micro ((^.))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Metrics (metrics, registerWaiMetrics)
import Safe (readMay)
import Say (say)
import Servant (Application)
import Servant.Auth.Server (defaultJWTSettings, fromSecret)
import SimpleSurvey.App (mkApp)
import SimpleSurvey.Config
  ( Config (..),
    Environment (..),
    katipLogger,
    makePool,
    setLogger,
  )
import SimpleSurvey.Database (doMigrations)
import SimpleSurvey.Logger (defaultLogEnv)
import System.Environment (lookupEnv)
import System.Remote.Monitoring
  ( Server (serverMetricStore, serverThreadId),
    forkServer,
  )

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> pure def
    Just s -> maybe (handleFailedRead s) pure (readMay s)
  where
    handleFailedRead s =
      error $
        mconcat
          [ "Failed to read [[",
            s,
            "]] for environment variable",
            env
          ]

tshow :: Show a => a -> Text
tshow = Text.pack . show

withConfig :: (Config -> IO a) -> IO a
withConfig runAppFromConfig = do
  say "Init.withConfig"
  port <- lookupSetting "PORT" (8081 :: Int)
  say $ "PORT: " <> tshow port
  env <- lookupSetting "ENV" Development
  say $ "ENV: " <> tshow env
  secret <- lookupSetting "SECRET" "secret"
  let jwt = fromSecret secret
  bracket defaultLogEnv (\x -> say "Closing katip scribes" >> Katip.closeScribes x) $ \logEnv -> do
    say "Got log env"
    !pool <- makePool env logEnv `onException` say "Exception while running makePool"
    say "Got pool"
    bracket (forkServer "localhost" 8082) (\x -> say "Closing ekg" >> do killThread $ serverThreadId x) $ \ekgServer -> do
      say "Forked ekg server"
      let store = serverMetricStore ekgServer
      say "Registered Wai metrics"
      metr <- M.initializeWith store
      say "Got metrics"
      runAppFromConfig
        Config
          { configPool = pool,
            configLogEnv = logEnv,
            configEnv = env,
            configPort = port,
            configMetrics = metr,
            configJWTSettings = defaultJWTSettings jwt,
            configEkgServer = serverThreadId ekgServer
          }

initialize :: Config -> IO Application
initialize cfg = do
  say "Init.initialize"
  waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
  say "Wai metrics registered"
  let kLogger = katipLogger (configLogEnv cfg)
  say "Katip logger was set"
  let logger = setLogger (configEnv cfg)
  say "Logger was set"
  bracket
    (say "Starting to run migrations")
    (\_ -> say "Migrations complete")
    $ \_ -> do
      say "Running migrations"
      runSqlPool doMigrations (configPool cfg) `catch` \(SomeException e) -> do
        say $
          mconcat
            [ "Exception in doMigrations of type:",
              tshow (typeOf e),
              ", shown:",
              tshow e
            ]
        throwIO e
      say "Completed runSqlPool"
  pure . kLogger . logger . metrics waiMetrics . mkApp $ cfg

shutDownApp :: Config -> IO ()
shutDownApp cfg = do
  _ <- Katip.closeScribes $ configLogEnv cfg
  Pool.destroyAllResources $ configPool cfg
  killThread (configEkgServer cfg)
  pure ()

runApp :: IO ()
runApp = do
  say "SimpleSurvey.runApp"
  withConfig $ \config -> do
    say "Acquire config"
    app <-
      initialize config
        `finally` say "Initialized config"
    say "Running app with set configuration"
    run (configPort config) app
      `finally` say "Server is closed"
