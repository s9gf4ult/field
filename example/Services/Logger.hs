module Services.Logger where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Field
import Control.Monad.State.Strict
import Data.Text as T
import Data.Text.IO as T
import GHC.Generics (Generic)


data LoggerService ctx m = LoggerService
  { logFunction :: Text -> FieldT ctx m ()
  } deriving Generic

pureLogger :: (Monad m) => Lens' (FieldS ctx m) [Text] -> LoggerService ctx m
pureLogger logLens = LoggerService go
  where
    go t = modify $ logLens %~ (t:)

printLogger :: (MonadBase IO m) => LoggerService ctx m
printLogger = LoggerService $ liftBase . T.putStrLn

logWithGetter :: (Monad m)
  => Getter (FieldS ctx m) (LoggerService ctx m)
  -> Text
  -> FieldT ctx m ()
logWithGetter l msg = do
  (LoggerService f) <- view l
  f msg
