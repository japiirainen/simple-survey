module SimpleSurvey.Base.Error
  ( Error (..),
  )
where

data Error a
  = AnyError
  | SpecificError a
  deriving (Show, Eq)
