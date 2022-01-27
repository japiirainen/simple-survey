{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module SimpleSurvey.Auth.API (authServer, AuthAPI, authAPI) where

import Control.Monad.Logger (logDebugNS)
import Control.Monad.Reader (MonadIO, liftIO)
import Data.Password.Bcrypt (PasswordCheck (..), checkPassword, mkPassword)
import Database.Persist.Postgresql (ConnectionPool)
import Servant
import Servant.Auth.Server
import SimpleSurvey.Auth.Domain.Commands.SignUpUser (signUpUser)
import SimpleSurvey.Auth.Domain.Models
  ( AuthUser (..),
    CheckPassword,
    ErrorGetUserByLogin (WrongPassword),
    Login (loginPassword),
    SavedUser (SavedUser),
    SignUp,
  )
import SimpleSurvey.Auth.Domain.Queries.LoginUser (loginUser)
import SimpleSurvey.Auth.Repository.Persist.Postgres
  ( createUserPostgres,
    getUserPostgres,
  )
import SimpleSurvey.Base.Error (Error (..))
import SimpleSurvey.Config (AppT)

type AcceptHeader returnContent =
  Headers
    '[ Header "Set-Cookie" SetCookie,
       Header "Set-Cookie" SetCookie
     ]
    returnContent

type RegistrationAppT m = AppT m (AcceptHeader NoContent)

type AuthAPIAction actionPayload m = CookieSettings -> JWTSettings -> actionPayload -> RegistrationAppT m

type LoginAPI =
  "login"
    :> ReqBody '[JSON] Login
    :> Verb 'POST 204 '[JSON] (AcceptHeader NoContent)

type RegisterAPI =
  "signup"
    :> ReqBody '[JSON] SignUp
    :> Verb 'POST 204 '[JSON] (AcceptHeader NoContent)

type AuthAPI =
  LoginAPI :<|> RegisterAPI

_err401 :: ServerError
_err401 =
  err401
    { errBody = "Error logging user in, please verify credentials"
    }

getAuthenticatedCookies :: (MonadIO m) => AuthAPIAction AuthUser m
getAuthenticatedCookies cookieSettings jwtSettings user = do
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
  case mApplyCookies of
    Nothing -> throwError _err401
    Just applyCookies -> pure $ applyCookies NoContent

checkLoginPassword :: (MonadIO m) => CheckPassword m
checkLoginPassword login (SavedUser username password) = do
  let pass = mkPassword (loginPassword login)
      hashedPass = password
      checkedPass = checkPassword pass hashedPass
  case checkedPass of
    PasswordCheckFail -> pure $ Left $ SpecificError WrongPassword
    PasswordCheckSuccess -> pure $ Right $ AuthUser username

loginAPI :: (MonadIO m) => ConnectionPool -> AuthAPIAction Login m
loginAPI pool cookieSettings jwtSettings loginInformation = do
  logDebugNS "Authentication" "loginAPI"
  authUser <- loginUser checkLoginPassword (getUserPostgres pool) loginInformation
  case authUser of
    Authenticated au -> getAuthenticatedCookies cookieSettings jwtSettings au
    _ -> throwError _err401

signUpAPI :: (MonadIO m) => ConnectionPool -> AuthAPIAction SignUp m
signUpAPI pool cookieSettings jwtSettings signUp = do
  logDebugNS "Authentication" "signUpAPI"
  authUser <- signUpUser (createUserPostgres pool) signUp
  case authUser of
    Authenticated au -> getAuthenticatedCookies cookieSettings jwtSettings au
    _ -> throwError _err401

authAPI :: Proxy AuthAPI
authAPI = Proxy

authServer ::
  (MonadIO m) =>
  CookieSettings ->
  JWTSettings ->
  ConnectionPool ->
  ServerT AuthAPI (AppT m)
authServer cs jwts pool = loginAPI pool cs jwts :<|> signUpAPI pool cs jwts