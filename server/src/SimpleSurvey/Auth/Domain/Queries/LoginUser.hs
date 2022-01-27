{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module SimpleSurvey.Auth.Domain.Queries.LoginUser
  ( validateUser,
    loginUser,
  )
where

import Servant.Auth.Server
import SimpleSurvey.Auth.Domain.Models (AuthUser (..), CheckPassword, ErrorGetUserByLogin (..), GetSavedUserByLogin, Login)
import SimpleSurvey.Base.Error (Error (..))

validateUser ::
  Monad m =>
  Either (Error ErrorGetUserByLogin) AuthUser ->
  m (AuthResult AuthUser)
validateUser = \case
  Left (SpecificError e) ->
    case e of
      UserDoesNotExist -> pure NoSuchUser
      _ -> pure Indefinite
  Left AnyError -> pure Indefinite
  Right user -> pure (Authenticated user)

loginUser ::
  Monad m =>
  CheckPassword m ->
  GetSavedUserByLogin m ->
  Login ->
  m (AuthResult AuthUser)
loginUser checkPassword getSavedUserByLogin login = do
  savedUser <- getSavedUserByLogin login
  case savedUser of
    Left e -> validateUser (Left e)
    Right user ->
      checkPassword login user >>= validateUser
