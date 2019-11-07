{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (liftM)
import           Control.Monad.IO.Class     (MonadIO (..), liftIO)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Function              ((&))
import           Web.Scotty                 (get, html, param, scotty)

newtype MaybeT' m a =
  MaybeT'
    { runMaybeT' :: m (Maybe a)
    }

instance Functor (MaybeT' m) where
  fmap = undefined

instance Applicative (MaybeT' m) where
  pure = undefined
  (<*>) = undefined

instance Monad (MaybeT' m) where
  (>>=) = undefined

instance MonadTrans MaybeT' where
  lift = undefined

instance (MonadIO m) => MonadIO (MaybeT' m) where
  liftIO :: IO a -> MaybeT' m a
  liftIO = lift . liftIO

newtype ReaderT' r m a =
  ReaderT'
    { runReaderT' :: r -> m a
    }

instance Functor (ReaderT' r m) where
  fmap = undefined

instance Applicative (ReaderT' r m) where
  pure = undefined
  (<*>) = undefined

instance Monad (ReaderT' r m) where
  (>>=) = undefined

-- instance MonadTrans (ReaderT' r) where
--   lift = undefined
instance (MonadIO m) => MonadIO (ReaderT' r m) where
  liftIO :: IO a -> ReaderT' r m a
  -- liftIO = lift . liftIO
  liftIO = ReaderT' . const . liftIO

newtype EitherT e m a =
  EitherT
    { runEitherT :: m (Either e a)
    }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT m_E_e_a) = EitherT $ (fmap . fmap) f m_E_e_a

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . pure
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT m_E_f <*> EitherT m_E_e_a = EitherT $ (<*>) <$> m_E_f <*> m_E_e_a

instance Monad m => Monad (EitherT e m) where
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT m_E_e_a >>= f =
    EitherT $ do
      either_e_a <- m_E_e_a
      case either_e_a of
        Right a -> runEitherT $ f a
        Left e  -> return $ Left e

instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a
  lift = EitherT . liftM Right

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT m_E_e_a) = EitherT $ fmap swapEither m_E_e_a
  where
    swapEither :: Either e a -> Either a e
    swapEither (Left e)  = Right e
    swapEither (Right a) = Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT m_E_a_b) = do
  either_a_b <- m_E_a_b
  case either_a_b of
    Left a  -> fa a
    Right b -> fb b

newtype StateT s m a =
  StateT
    { runStateT :: s -> m (a, s)
    }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT s2m_as) = StateT . ((fmap . fmap) (go f)) $ s2m_as
    where
      go :: (a -> b) -> (a, s) -> (b, s)
      go g (a, s) = (g a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT (\s -> pure (a, s))
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT s2m_fs <*> StateT s2m_as =
    StateT $ \s -> do
      (a, s') <- s2m_as s
      (f, s'') <- s2m_fs s'
      pure (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT s2m_as >>= f =
    StateT
      (\s -> do
         (a, s') <- s2m_as s
         let (StateT s2m_bs) = f a
         s2m_bs s')

instance MonadTrans (StateT s) where
  lift :: (Monad m) => m a -> StateT s m a
  lift ma = StateT (\s -> (\a -> (a, s)) <$> ma)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO = lift . liftIO

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
-- embedded = MaybeT . ExceptT . ReaderT . const . pure . Right . Just $ 1
embedded = 1 & Just & Right & pure & const & ReaderT & ExceptT & MaybeT

main :: IO ()
main =
  scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    liftIO (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
