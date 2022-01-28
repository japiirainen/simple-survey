module SS.Database.Users
  ( getByEmail,
    get,
    insert,
  )
where

import SS.Database (Fetch, runQuery)
import SS.Model.User (Email (..), User (..), UserId)
import SS.Types.Entity (Entity)
import Prelude hiding (id)

get :: UserId -> Fetch (Entity User)
get id = do
  [x] <- runQuery "select * from users where id = ?" [id]
  return x

getByEmail :: Email -> Fetch (Entity User)
getByEmail email = do
  [x] <- runQuery "select * from users where email = ?" [email]
  return x

insert :: User -> Fetch (Entity User)
insert User {..} = do
  [x] <-
    runQuery
      "insert into users (email, hashed_password) values (?, ?) returning *"
      (userEmail, userHashedPassword)
  return x