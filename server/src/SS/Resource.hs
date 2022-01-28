module SS.Resource
  ( API,
    proxy,
    routes,
  )
where

import SS.Resource.Session (SessionAPI, sessionAPI)
import SS.Resource.User (UserAPI, userAPI)
import SS.Types (App)
import Servant (Proxy (Proxy), ServerT, (:<|>) (..))

type API =
  SessionAPI
    :<|> UserAPI

routes :: ServerT API App
routes = sessionAPI :<|> userAPI

proxy :: Proxy API
proxy = Proxy