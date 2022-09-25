{-# LANGUAGE OverloadedStrings #-}

-- | Outdated implementation of the Sqlite-based 'Disk' which was using
-- a record rather than a sum type to shuffle around operations like 'put'.
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
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
import qualified Database.SQLite3 as Sql
import qualified System.IO.Error as Sys
import qualified System.Directory as Sys

{-----------------------------------------------------------------------------
    Disk
------------------------------------------------------------------------------}
-- | Represents the on-disk storage where 'DiskBytes' are stored.
data Disk = Disk
    { path :: FilePath
    , db :: Sql.Database
    , chan :: STM.TChan Cmd
    }

-- | Obtain the size of the 'Disk' in bytes.
-- Here, this is the file size.
getDiskSize :: Disk -> IO Integer
getDiskSize = Sys.getFileSize . path

{-
-- | Create a new file and use it for storing 'DiskBytes'.
newDisk :: FilePath -> IO Disk
newDisk path = do
    -- check that file does not exist yet
    db <- Sql.open (T.pack path)
    -- make file read-only
    
    sql <- initDatabase db
    pure $ Disk{path,db,sql}
-}

-- | Create a new file and use it for storing 'DiskBytes'.
-- Delete the file afterwards.
withDiskSqlite :: FilePath -> (Disk -> IO a) -> IO a
withDiskSqlite path action =
    withFile path $ \_ ->
    withDatabase path $ \db ->
    withSql db $ \sql ->
    withThread sql $ \(_,chan) ->
        action $ Disk{path,db,chan}
  where
    withDatabase path = bracket (Sql.open $ T.pack path) Sql.close
    withThread db = bracket (mkDatabaseThread db) (killThread . fst)

    withFile path = bracket
        (throwIfAlreadyExists path)
        (\_ -> Sys.removeFile path)

-- | Throw an 'IOError' that indicates that the file already exists.
throwIfAlreadyExists :: FilePath -> IO ()
throwIfAlreadyExists path = do
    b <- Sys.doesFileExist path
    when b $ Sys.ioError $ Sys.mkIOError
        Sys.alreadyExistsErrorType "Creating 'Disk'" Nothing (Just path)

-- | Operations that we would like to perform on a storage type 'db'
data Ops db = Ops
    { put :: db -> ByteString -> IO Int64
    , get :: db -> Int64 -> IO ByteString
    , delete :: db -> Int64 -> IO ()
    }

diskOps :: Ops Disk
diskOps = Ops
    { put = \Disk{chan} -> put chanOps chan
    , get = \Disk{chan} -> get chanOps chan
    , delete = \Disk{chan} -> delete chanOps chan
    }

{-----------------------------------------------------------------------------
    Worker Thread
------------------------------------------------------------------------------}
data Cmd
    = Put !ByteString (STM.TMVar Int64)
    | Get !Int64 (STM.TMVar ByteString)
    | Delete !Int64

mkDatabaseThread :: SqlCmds -> IO (ThreadId, STM.TChan Cmd)
mkDatabaseThread sql = do
    chan <- STM.newTChanIO
    threadId <- forkIO $ forever $ do
        cmd <- STM.atomically $ STM.readTChan chan
        case cmd of
            Put a k -> do
                b <- put sqlOps sql a
                STM.atomically $ STM.putTMVar k b
            Get a k -> do
                b <- get sqlOps sql a
                STM.atomically $ STM.putTMVar k b
            Delete a -> do
                delete sqlOps sql a
    pure (threadId, chan)

chanOps :: Ops (STM.TChan Cmd)
chanOps = Ops
    { put = \chan a -> do
        k <- STM.newEmptyTMVarIO
        STM.atomically $ STM.writeTChan chan $ Put a k
        STM.atomically $ STM.takeTMVar k
    , get = \chan a -> do
        k <- STM.newEmptyTMVarIO
        STM.atomically $ STM.writeTChan chan $ Get a k
        STM.atomically $ STM.takeTMVar k
    , delete = \chan a -> do
        STM.atomically $ STM.writeTChan chan $ Delete a
    }

{-----------------------------------------------------------------------------
    Sql
------------------------------------------------------------------------------}
data SqlCmds = SqlCmds
    { insert_ :: Sql.Statement
    , read_ :: Sql.Statement
    , delete_ :: Sql.Statement
    , counter :: IORef Int64
    }

withSql :: Sql.Database -> (SqlCmds -> IO a) -> IO a
withSql db = bracket (initSql db) finalizeSql

initSql :: Sql.Database -> IO SqlCmds
initSql db = do
    Sql.exec db "CREATE TABLE db ( idx INTEGER PRIMARY KEY, val BLOB );"
    insert_ <- Sql.prepare db "INSERT INTO db VALUES (?1,?2);"
    read_   <- Sql.prepare db "SELECT val FROM db WHERE idx = ?1;"
    delete_ <- Sql.prepare db "DELETE FROM db WHERE idx = ?1;"
    counter <- newIORef 0
    pure $ SqlCmds{insert_,read_,delete_,counter}

finalizeSql :: SqlCmds -> IO ()
finalizeSql SqlCmds{insert_,read_,delete_} = do
    mapM_ Sql.finalize [insert_, read_, delete_]

sqlOps :: Ops SqlCmds
sqlOps = Ops
    { put = \SqlCmds{insert_=s,counter} val -> do
        idx <- readIORef counter
        writeIORef counter (idx+1)
        Sql.bind s [Sql.SQLInteger idx, Sql.SQLBlob val]
        _ <- Sql.stepNoCB s
        Sql.reset s
        Sql.clearBindings s
        pure idx

    , get = \SqlCmds{read_=s} idx -> do
        Sql.bind s [Sql.SQLInteger idx]
        Sql.Row <- Sql.stepNoCB s
        val <- Sql.columnBlob s 0
        Sql.reset s
        Sql.clearBindings s
        pure val

    , delete = \SqlCmds{delete_=s} idx -> do
        Sql.bind s [Sql.SQLInteger idx]
        Sql.Done <- Sql.stepNoCB s
        Sql.reset s
        Sql.clearBindings s
    }
