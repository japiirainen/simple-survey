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
import Database.Persist.Postgresql (ConnectionPool)
import Network.Wai
import Servant
import Servant.Auth.Server
import SimpleSurvey.Auth.API
import SimpleSurvey.Config
  ( AppT (runAppT),
    Config (configJWTSettings, configPool),
  )

-- * api

-- This is the API definition

-- type Authorized = Auth '[Cookie] AuthUser

type API = AuthAPI

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
server pool jwtSettings = authServer defaultCookieSettings jwtSettings pool

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings {cookieIsSecure = NotSecure, cookieSameSite = SameSiteStrict, cookieXsrfSetting = Nothing}

-- Creates IO application from API definition and server
mkApp :: Config -> Application
mkApp config = do
  let ctx = cookieSettings :. configJWTSettings config :. EmptyContext
  serveWithContext api ctx (appToServer config)
