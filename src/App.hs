module App where

newtype App a = App {runApp :: ReaderT Ghci IO a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Ghci, MonadIO)

printIO :: Show s => s -> App ()
printIO = liftIO . print

execApp :: Ghci -> App a -> IO a
execApp ghci (App app) = runReaderT app ghci