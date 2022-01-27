{-# LANGUAGE RankNTypes #-}

module SimpleSurvey.Auth.Repository.UserRepository (UserRepository (..)) where

import SimpleSurvey.Auth.Domain.Models (CreateUserBySignup, GetSavedUserByLogin)

data UserRepository m = UserRepository
  { getUserByLogin :: GetSavedUserByLogin m,
    createUserBySignup :: CreateUserBySignup m
  }