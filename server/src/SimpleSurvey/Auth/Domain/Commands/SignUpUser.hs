{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module SimpleSurvey.Auth.Domain.Commands.SignUpUser
  ( validateUser,
    signUpUser,
  )
where

import Control.Monad ((>=>))
import Servant.Auth.Server
import SimpleSurvey.Auth.Domain.Models (AuthUser (..), CreateUserBySignup, ErrorInsertUser, SignUp)
import SimpleSurvey.Base.Error (Error)

validateUser ::
  Monad m =>
  Either (Error ErrorInsertUser) AuthUser ->
  m (AuthResult AuthUser)
validateUser = \case
  Left _ -> pure Indefinite
  Right user -> pure (Authenticated user)

signUpUser ::
  Monad m =>
  CreateUserBySignup m ->
  SignUp ->
  m (AuthResult AuthUser)
signUpUser create = create >=> validateUser
