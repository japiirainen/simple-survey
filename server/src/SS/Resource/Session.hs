module SS.Resource.Session where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ask)
import Crypto.KDF.BCrypt (validatePassword)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as B
import GHC.Generics (Generic)
import SS.Database.Users (getByEmail)
import SS.Model.Credentials (Credentials (..), unPassword)
import SS.Model.User (User (..), UserId, unHashedPassword)
import qualified SS.Types as T
import SS.Types.Entity (Entity (..))
import Servant
  ( Delete,
    Header,
    Headers,
    JSON,
    NoContent (..),
    Post,
    ReqBody,
    ServerT,
    err401,
    noHeader,
    throwError,
    (:<|>) (..),
    (:>),
  )
import Servant.Auth.Server (FromJWT, SetCookie, ToJWT, acceptLogin)

newtype Session
  = Session UserId
  deriving (Eq, Show, Read, Generic)

instance FromJSON Session

instance ToJSON Session

instance FromJWT Session

instance ToJWT Session

type PostSessionR =
  "session"
    :> ReqBody '[JSON] Credentials
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

postSession ::
  Credentials ->
  T.App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
postSession Credentials {..} = do
  user <- T.runDB $ getByEmail credentialsEmail
  T.AppContext {..} <- ask
  case user of
    Just (Entity userId User {..}) ->
      let hash = unHashedPassword userHashedPassword
          valid = validatePassword (B.pack $ unPassword credentialsPassword) (B.pack hash)
       in if valid
            then do
              mApplyCookies <-
                liftIO $ acceptLogin appContextCookieSettings appContextJWTSettings (Session userId)
              case mApplyCookies of
                Nothing -> throwError err401
                Just applyCookies -> return $ applyCookies NoContent
            else throwError err401
    _ -> throwError err401

type DeleteSessionR =
  "session"
    :> Delete '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

deleteSession ::
  T.App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
deleteSession = return ((noHeader . noHeader) NoContent)

type SessionAPI =
  PostSessionR
    :<|> DeleteSessionR

sessionAPI :: ServerT SessionAPI T.App
sessionAPI = postSession :<|> deleteSession