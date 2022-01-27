{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module SimpleSurvey.Auth.Domain.Models where

import Data.Aeson
import Data.Password.Bcrypt (Bcrypt, PasswordHash (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Auth.Server
import SimpleSurvey.Base.Error (Error)

type Password = Text

type Username = Text

type HashedPassword = PasswordHash Bcrypt

newtype AuthUser = AuthUser Username
  deriving (Show, Generic)

instance ToJSON AuthUser

instance FromJSON AuthUser

instance ToJWT AuthUser

instance FromJWT AuthUser

data SavedUser = SavedUser Username HashedPassword
  deriving (Show, Generic)

data SignUp = SignUp
  { signUpUserName :: !Username,
    signUpPassword :: !Password
  }
  deriving (Show, Generic)

instance ToJSON SignUp

instance FromJSON SignUp

data Login = Login
  { loginUserName :: !Username,
    loginPassword :: !Password
  }
  deriving (Show, Generic)

instance ToJSON Login

instance FromJSON Login

type Authorized = Auth '[Cookie, JWT] AuthUser

data ErrorInsertUser = InserUserConflict
  deriving (Eq, Show)

type CreateUserBySignup m =
  Monad m =>
  SignUp ->
  m (Either (Error ErrorInsertUser) AuthUser)

data ErrorGetUserByLogin
  = UserDoesNotExist
  | ReplicatedUser
  | WrongPassword
  deriving (Eq, Show)

type GetSavedUserByLogin m =
  Login ->
  m (Either (Error ErrorGetUserByLogin) SavedUser)

type CheckPassword m =
  Login ->
  SavedUser ->
  m (Either (Error ErrorGetUserByLogin) AuthUser)
