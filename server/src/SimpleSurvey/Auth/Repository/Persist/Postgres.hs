{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module SimpleSurvey.Auth.Repository.Persist.Postgres
  ( postgresUserRepository,
    getUserPostgres,
    createUserPostgres,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger (..), logDebugNS)
import Data.Password.Bcrypt (hashPassword)
import Data.Password.Types (mkPassword)
import Database.Persist.Postgresql
  ( ConnectionPool,
    Entity (entityVal),
    insertBy,
    runSqlPool,
    selectList,
    (==.),
  )
import SimpleSurvey.Auth.Domain.Models
  ( AuthUser (..),
    CreateUserBySignup,
    ErrorGetUserByLogin (ReplicatedUser, UserDoesNotExist),
    ErrorInsertUser (InsertUserConflict),
    GetSavedUserByLogin,
    Login (loginUserName),
    SavedUser (..),
    SignUp (signUpPassword, signUpUserName),
  )
import SimpleSurvey.Auth.Repository.UserRepository (UserRepository (..))
import SimpleSurvey.Base.Error (Error (..))
import SimpleSurvey.Database as DB
  ( EntityField (UserIdent),
    User (User, userIdent, userPassword),
  )

mapUserToAuthUser :: User -> AuthUser
mapUserToAuthUser user = AuthUser (userIdent user)

mapUserToSavedUser :: User -> SavedUser
mapUserToSavedUser user = SavedUser (userIdent user) (userPassword user)

getUserPostgres :: (MonadIO m, MonadLogger m) => ConnectionPool -> GetSavedUserByLogin m
getUserPostgres pool login = do
  logDebugNS "web" "getUserPostgres"

  let query = selectList [UserIdent ==. loginUserName login] []
  users <- liftIO $ runSqlPool query pool

  case length users of
    0 -> pure $ Left (SpecificError UserDoesNotExist)
    1 -> pure $ Right $ head $ map (mapUserToSavedUser . entityVal) users
    _ -> pure $ Left (SpecificError ReplicatedUser)

createUserPostgres :: (MonadIO m, MonadLogger m) => ConnectionPool -> CreateUserBySignup m
createUserPostgres pool signUp = do
  logDebugNS "web" "getUserPostgres"
  hashedPsw <- hashPassword $ mkPassword $ signUpPassword signUp
  let username = signUpUserName signUp
      user = User username hashedPsw
      query = insertBy user
  result <- liftIO $ runSqlPool query pool

  case result of
    Left _ -> pure $ Left (SpecificError InsertUserConflict)
    Right _ -> pure $ Right (mapUserToAuthUser user)

postgresUserRepository :: (MonadIO m, MonadLogger m) => ConnectionPool -> UserRepository m
postgresUserRepository pool = UserRepository (getUserPostgres pool) (createUserPostgres pool)