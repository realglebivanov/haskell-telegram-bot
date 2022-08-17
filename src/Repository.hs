{-# LANGUAGE TypeFamilies #-}

module Repository where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, unliftIO)
import Control.Monad.Logger (MonadLoggerIO, runStdoutLoggingT)
import Control.Monad.Reader (join, runReaderT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Database.Persist.Class.PersistEntity (Entity (entityVal), Filter, Key, PersistEntity (PersistEntityBackend), SelectOpt, Update)
import Database.Persist.Postgresql
  ( ConnectionString,
    PersistStoreRead (get),
    PersistStoreWrite (delete, insert, updateGet),
    selectList,
    withPostgresqlConn,
  )
import Database.Persist.SqlBackend.Internal (SqlBackend)
import Fmt ((+|), (|+))
import System.Environment (getEnv)

listRecords ::
  (PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
  [Filter a] ->
  [SelectOpt a] ->
  IO [a]
listRecords filters selectOpts = map entityVal <$> exec (selectList filters selectOpts)

getRecord key = exec $ get key

insertRecord record = exec $ insert record

deleteRecord key = exec $ delete key

updateRecord key updates = exec $ updateGet key updates

exec action = do
  connectionString <- buildConnectionString
  runStdoutLoggingT $ withPostgresqlConn connectionString (runReaderT action)

buildConnectionString :: IO ConnectionString
buildConnectionString =
  liftIO $ do
    host <- getEnv "PG_HOST"
    port <- getEnv "PG_PORT"
    user <- getEnv "PG_USER"
    password <- getEnv "PG_PASSWORD"
    database <- getEnv "PG_DATABASE"
    return $ "host=" +| host |+ "port=" +| port |+ " user=" +| user |+ " dbname=" +| database |+ " password=" +| password |+ ""
