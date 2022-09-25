{-# LANGUAGE OverloadedStrings #-}

-- | Implementation of an Sqlite-based 'Disk'.
module System.Mem.Disk.Sqlite where

import Control.Concurrent
    ( forkIO, killThread, ThreadId )
import Control.Exception
    ( bracket )
import Control.Monad
    ( forever, when )
import Data.ByteString
    ( ByteString )
import Data.Int
    ( Int64 )
import Data.IORef

import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
import qualified Database.SQLite3 as Sql
import qualified System.IO.Error as Sys
import qualified System.Directory as Sys

import qualified System.Mem.Disk.DiskApi as DiskApi

{-----------------------------------------------------------------------------
    File system helpers
------------------------------------------------------------------------------}
withFile :: FilePath -> IO a -> IO a
withFile path action = bracket
    (throwIfAlreadyExists path)
    (\_ -> Sys.removeFile path)
    (\_ -> action)

-- | Throw an 'IOError' that indicates that the file already exists.
throwIfAlreadyExists :: FilePath -> IO ()
throwIfAlreadyExists path = do
    b <- Sys.doesFileExist path
    when b $ Sys.ioError $ Sys.mkIOError
        Sys.alreadyExistsErrorType "Creating 'Disk'" Nothing (Just path)

{-----------------------------------------------------------------------------
    Disk
------------------------------------------------------------------------------}
data Disk = Disk
    { path :: FilePath
    , chan :: STM.TChan (Cmd STM.TMVar)
    , counter :: IORef Int64
    }

-- | Obtain the size of the 'Disk' in bytes.
-- Here, this is the file size.
getDiskSize_ :: Disk -> IO Integer
getDiskSize_ = Sys.getFileSize . path

-- | Create a new file and use it for storing
-- t'System.Mem.Disk.DiskBytes'.
--
-- Throw an error if the file already exists,
-- delete the file after use.
withDiskSqlite :: FilePath -> (DiskApi.Disk -> IO a) -> IO a
withDiskSqlite path action =
    withFile path $
    withDatabase path $ \db ->
    withSql db $ \sql ->
    withThread sql $ \chan -> do
        counter <- newIORef 0
        action $ mkDiskApi $ Disk{path,chan,counter}
  where
    withDatabase path = bracket (Sql.open $ T.pack path) Sql.close

mkDiskApi :: Disk -> DiskApi.Disk
mkDiskApi disk = DiskApi.Disk
    { DiskApi.put = put_ disk
    , DiskApi.get = get_ disk
    , DiskApi.delete = delete_ disk
    , DiskApi.getDiskSize = getDiskSize_ disk
    }

{-----------------------------------------------------------------------------
    Disk operations
------------------------------------------------------------------------------}
-- | Operations to be performed on the database
data Cmd cont
    = Put !Int64 ByteString
    | Get !Int64 (cont ByteString)
    | Delete !Int64

put_ :: Disk -> ByteString -> IO Int64
put_ Disk{chan,counter} bytes = do
    index <- atomicModifyIORef' counter (\x -> (x+1,x))
    STM.atomically $ STM.writeTChan chan $ Put index bytes
    pure index

get_ :: Disk -> Int64 -> IO ByteString
get_ Disk{chan} index = do
    k <- STM.newEmptyTMVarIO
    STM.atomically $ STM.writeTChan chan $ Get index k
    STM.atomically $ STM.takeTMVar k

delete_ :: Disk -> Int64 -> IO ()
delete_ Disk{chan} index =
    STM.atomically $ STM.writeTChan chan $ Delete index

{-----------------------------------------------------------------------------
    Worker Thread
------------------------------------------------------------------------------}
-- | Worker thread for sequencing SQL commands.
withThread :: SqlCmds -> (STM.TChan (Cmd STM.TMVar) -> IO a) -> IO a
withThread sql action =
    bracket (mkDatabaseThread sql) (killThread . fst) (\(_,c) -> action c)

mkDatabaseThread :: SqlCmds -> IO (ThreadId, STM.TChan (Cmd STM.TMVar))
mkDatabaseThread sql = do
    chan <- STM.newTChanIO
    threadId <- forkIO $ forever $
        cmdSql sql =<< STM.atomically (STM.readTChan chan)
    pure (threadId, chan)

{-----------------------------------------------------------------------------
    Sql
------------------------------------------------------------------------------}
data SqlCmds = SqlCmds
    { sput_ :: Sql.Statement
    , sget_ :: Sql.Statement
    , sdelete_ :: Sql.Statement
    }

withSql :: Sql.Database -> (SqlCmds -> IO a) -> IO a
withSql db = bracket (initSql db) finalizeSql

initSql :: Sql.Database -> IO SqlCmds
initSql db = do
    Sql.exec db "CREATE TABLE db ( ix INTEGER PRIMARY KEY, bytes BLOB );"
    sput_ <- Sql.prepare db "INSERT INTO db VALUES (?1,?2);"
    sget_ <- Sql.prepare db "SELECT bytes FROM db WHERE ix = ?1;"
    sdelete_ <- Sql.prepare db "DELETE FROM db WHERE ix = ?1;"
    pure $ SqlCmds{sput_,sget_,sdelete_}

finalizeSql :: SqlCmds -> IO ()
finalizeSql SqlCmds{sput_,sget_,sdelete_} = do
    mapM_ Sql.finalize [sput_, sget_, sdelete_]

cmdSql :: SqlCmds -> Cmd STM.TMVar -> IO ()
cmdSql SqlCmds{sput_,sget_,sdelete_} cmd = case cmd of
    Put index bytes -> do
        let s = sput_
        Sql.bind s [Sql.SQLInteger index, Sql.SQLBlob bytes]
        _ <- Sql.stepNoCB s
        reset s

    Get index k -> do
        let s = sget_
        Sql.bind s [Sql.SQLInteger index]
        Sql.Row <- Sql.stepNoCB s
        bytes <- Sql.columnBlob s 0
        reset s
        STM.atomically $ STM.putTMVar k bytes

    Delete index -> do
        let s = sdelete_
        Sql.bind s [Sql.SQLInteger index]
        Sql.Done <- Sql.stepNoCB s
        reset s

  where
    reset s = Sql.reset s >> Sql.clearBindings s
