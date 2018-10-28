module Services.UserAuth where

import Control.Monad.Field

data User = Anon | Registered

data UserAuthService m = UserAuthService
  { currentUser :: m User
  , register    :: User -> m ()
  }

-- | Changes user with MonadState instance
pureUserAuth :: UserAuth m
pureUserAuth = UserAuth
  { currentUser = return Anon
  , register u =
