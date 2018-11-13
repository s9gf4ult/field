module Services.UserAuth where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Field
import Control.Monad.State.Strict
import Data.Generics.Product
import Data.IORef
import Data.Text as T
import GHC.Generics (Generic)
import Services.Logger


data User = Anon | Registered
  deriving Show

data UserAuthService ctx m = UserAuthService
  { currentUser :: FieldT ctx m User
  , register    :: User -> FieldT ctx m ()
  } deriving (Generic)


-- | Changes user with MonadState instance of FieldT
pureUserAuth
  :: forall m ctx
  . (Monad m)
  => Lens' (FieldS ctx m) (UserAuthService ctx m)
  ->  UserAuthService ctx m
pureUserAuth serviceLens = UserAuthService
  { currentUser = return Anon
  , register    = \u -> modify
    $ serviceLens . field' @"currentUser" .~ return u
  }

ioUserAuth :: (Monad m, MonadBase IO m) => IO (UserAuthService ctx m)
ioUserAuth = do
  ref <- newIORef Anon
  let
    res = UserAuthService
      { currentUser = do
          liftBase $ readIORef ref
      , register = \u -> do
          liftBase $ writeIORef ref u
      }
  return res

-- | Wrap user service with logging
loggingUserAuth
  :: (Monad m)
  => Getter (FieldS ctx m) (LoggerService ctx m)
  -> UserAuthService ctx m
  -> UserAuthService ctx m
loggingUserAuth loggerLens uas = UserAuthService
  { currentUser = do
      u <- uas ^. field' @"currentUser"
      logWithGetter loggerLens $ "Current user is " <> T.pack (show u)
      return u
  , register = \u -> do
      (uas ^. field' @"register") u
      logWithGetter loggerLens $ "User registered " <> T.pack (show u)
  }
