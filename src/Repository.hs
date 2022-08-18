{-# LANGUAGE TypeFamilies #-}

module Repository where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, unliftIO)
import Control.Monad.Logger (MonadLoggerIO, runStdoutLoggingT)
import Control.Monad.Reader (join, runReaderT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Database.Persist.Class (deleteWhere, updateWhere)
import Database.Persist.Class.PersistEntity (Entity (entityVal), Filter, Key, PersistEntity (PersistEntityBackend), SelectOpt, Update)
import Database.Persist.Postgresql
  ( ConnectionString,
    PersistStoreRead (get),
    PersistStoreWrite (insert),
    selectList,
    withPostgresqlConn,
  )
import Database.Persist.SqlBackend.Internal (SqlBackend)
import Fmt ((+|), (|+))
import Models.Book
import System.Environment (getEnv)

listRecords ::
  (PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
  [Filter a] ->
  [SelectOpt a] ->
  IO [Entity a]
listRecords filters selectOpts = exec (selectList filters selectOpts)

getRecord key = exec $ get key

insertRecord record = exec $ insert record

deleteRecords filters = exec $ deleteWhere filters

updateRecords filters updates = exec $ updateWhere filters updates

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
    return $ "host=" +| host |+ " port=" +| port |+ " user=" +| user |+ " dbname=" +| database |+ " password=" +| password |+ ""
