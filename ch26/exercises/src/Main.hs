{-# LANGUAGE InstanceSigs #-}

module Main where

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

main :: IO ()
main = do
  putStrLn "hello world"
