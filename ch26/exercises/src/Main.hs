{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (guard, liftM)
import           Control.Monad.IO.Class     (MonadIO (..), liftIO)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Reader (Reader, ReaderT (..), runReader)
import           Data.Function              ((&))
import           Data.Functor.Identity      (Identity (..))
import           Data.IORef                 (IORef, newIORef)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty                 (get, html, param, scotty)
import           Web.Scotty.Trans           (ActionT, ScottyT, scottyT)

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

-- Hypothetical exercise (p1062)
-- I think they are same. As:
-- ```
-- ReaderT r Maybe a = ReaderT {runReaderT :: r -> Maybe a}
-- MaybeT (Reader r) a = MaybeT {runMaybeT :: (Reader r) (Maybe a) :: Reader r (Maybe a) :: r -> Maybe a}
-- ```
rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . subtract 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT
    (\x -> do
       putStrLn $ "Hi: " <> show x
       return $ x + 1)

sPrintIncAccum :: (Num s, Show s) => StateT s IO String -- StateT {runStateT :: s -> IO (String, s)}
sPrintIncAccum =
  StateT
    (\s -> do
       putStrLn $ "Hi: " <> show s
       pure (show s, s + 1))

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn $ "Good, was very excite: " ++ e

main' :: IO ()
main' = do
  print $ runReader rDec (1 :: Integer) == 0
  print $ fmap (runReader rDec) [1 .. 10 :: Integer] == [0 .. 9]
  print $ runReaderT rShow (1 :: Integer) == "1"
  print $ fmap (runReaderT rShow) ([1 .. 10 :: Integer]) == ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
  runReaderT rPrintAndInc (1 :: Integer) >>= print
  traverse (runReaderT rPrintAndInc) [1 .. 10 :: Integer] >>= print
  runStateT sPrintIncAccum (10 :: Integer) >>= print
  mapM (runStateT sPrintIncAccum) [1 .. 5 :: Integer] >>= print

-- TODO: chapter exercises from here till end (definitions also)
data Config =
  Config
    { counts :: IORef (M.Map Text Integer)
    , prefix :: Text
    }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = undefined

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    let key' = mappend undefined unprefixed
    newInteger <- undefined
    html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

main'' :: IO ()
main'' = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = undefined
      runR = undefined
  scottyT 3000 runR app
