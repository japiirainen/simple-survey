{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SimpleSurvey.App
  ( mkApp,
  )
where

import Control.Monad.Reader (MonadIO, runReaderT)
import Data.Text (Text)
import Database.Persist.Postgresql (ConnectionPool)
import Network.Wai
import Servant
  ( Context (EmptyContext, (:.)),
    Get,
    Handler (Handler),
    HasServer (ServerT, hoistServerWithContext),
    IsSecure (NotSecure),
    PlainText,
    Proxy (..),
    Server,
    serveWithContext,
  )
import Servant.Auth.Server
  ( CookieSettings (cookieIsSecure, cookieSameSite, cookieXsrfSetting),
    JWTSettings,
    SameSite (SameSiteStrict),
    defaultCookieSettings,
  )
import SimpleSurvey.Config
  ( AppT (runAppT),
    Config (configJWTSettings, configPool),
  )

-- * api

-- This is the API definition

type API = Get '[PlainText] Text

context :: Proxy '[CookieSettings, JWTSettings]
context = Proxy

api :: Proxy API
api = Proxy

-- * general

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runAppT appt) cfg

appToServer :: Config -> Server API
appToServer cfg = hoistServerWithContext api context (convertApp cfg) $ server (configPool cfg) (configJWTSettings cfg)

-- Creates server definition for API, which is the one that adds the logic to the typed specification
server :: (MonadIO m) => ConnectionPool -> JWTSettings -> ServerT API (AppT m)
server _pool _jwtSettings = return "Hello world"

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings {cookieIsSecure = NotSecure, cookieSameSite = SameSiteStrict, cookieXsrfSetting = Nothing}

-- Creates IO application from API definition and server
mkApp :: Config -> Application
mkApp config = do
  let ctx = cookieSettings :. configJWTSettings config :. EmptyContext
  serveWithContext api ctx (appToServer config)
