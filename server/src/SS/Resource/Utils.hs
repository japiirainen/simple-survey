module SS.Resource.Utils
  ( orElseThrow,
  )
where

import Control.Monad.Error.Class (MonadError)
import Servant (ServerError, throwError)

orElseThrow :: (MonadError ServerError m) => ServerError -> Maybe a -> m a
orElseThrow err = maybe (throwError err) return