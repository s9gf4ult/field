module Control.Monad.Field where

import Control.Monad.Base
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT(..))


-- | Struct of field monad with applied argument
type FieldS s m = s (FieldT s m)

newtype FieldT s m a = FieldT (StateT (FieldS s m) m a)
  deriving (Functor, Applicative, Monad)

deriving instance (Monad m, MonadBase b m) => MonadBase b (FieldT s m)

deriving instance (Monad m) => MonadState (FieldS s m) (FieldT s m)

instance (Monad m) => MonadReader (FieldS s m) (FieldT s m) where
  ask = get
  local f ma = freeze $ do
    state $ ((),) <$> f
    ma
  reader f = f <$> ask

runFieldT :: FieldS s m -> FieldT s m a -> m (a, FieldS s m)
runFieldT s (FieldT ma) = runStateT ma s

evalFieldT :: (Functor m) => FieldS s m -> FieldT s m a -> m a
evalFieldT s m = fmap fst $ runFieldT s m

-- | Discards nested computations
freeze :: (Monad m) => FieldT s m a -> FieldT s m a
freeze ma = do
  s <- get
  res <- ma
  put s
  return res
