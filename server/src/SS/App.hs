module SS.App
  ( startApp,
  )
where

import Network.Wai.Handler.Warp (run)
import SS.Database (initializeDatabase)
import SS.Resource (API, proxy, routes)
import SS.Types (AppContext (..), convert)
import Servant
  ( Application,
    Context (EmptyContext, (:.)),
    Proxy (Proxy),
    Server,
    hoistServerWithContext,
    serveWithContext,
  )
import Servant.Auth.Server
  ( CookieSettings,
    IsSecure (NotSecure),
    JWTSettings,
    cookieIsSecure,
    def,
    defaultJWTSettings,
    generateKey,
  )

startApp :: IO ()
startApp = do
  myKey <- generateKey
  pool <- initializeDatabase
  -- in production, the cookie should be secure
  let ctx =
        AppContext
          { appContextPool = pool,
            appContextPort = 8081,
            appContextApproot = "localhost",
            appContextCookieSettings = def {cookieIsSecure = NotSecure},
            appContextJWTSettings = defaultJWTSettings myKey
          }
  run (appContextPort ctx) $ createApp ctx

createApp :: AppContext -> Application
createApp appContext@AppContext {..} =
  let ctx = (appContextCookieSettings :. appContextJWTSettings :. EmptyContext)
   in serveWithContext proxy ctx $ createServer appContext

createServer :: AppContext -> Server API
createServer appContext = hoistServerWithContext proxy context (convert appContext) routes

context :: Proxy '[CookieSettings, JWTSettings]
context = Proxy