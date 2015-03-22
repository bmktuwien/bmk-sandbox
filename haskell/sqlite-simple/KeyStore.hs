{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Types
import Control.Monad
import Control.Applicative
import Control.Concurrent.MVar
import Data.Word
import Data.IORef
import qualified Data.Text           as T
import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM

type KeyParamId = Word16
type ExpireTime = Word64

type PublicKey  = B.ByteString
type PrivateKey = B.ByteString

data SrvKeyParams = SrvKeyParams
    { skpId         :: !KeyParamId
    , skpTimeout    :: !ExpireTime -- ^ idle timeout
    , skpSrvPrivKey :: !PrivateKey
    , skpSrvPubKey  :: !PublicKey
    , skpClnPubKey  :: !PublicKey
    } deriving(Show)

data KeyStore = KeyStore
  { ksConnection :: Connection
  , ksCache      :: IORef (HM.HashMap KeyParamId SrvKeyParams)
  }

type KeyStoreMVar = MVar KeyStore

instance FromRow SrvKeyParams where
  fromRow = SrvKeyParams <$> field <*> field <*> field <*> field <*> field

addQuery :: Query
addQuery = "INSERT INTO keystore                       \
            \  (timeout, server_public_key,            \
            \   server_private_key, client_public_key) \
            \  VALUES (?, ?, ?, ?)"

updateQuery :: Query
updateQuery = "UPDATE keystore SET       \
              \ timeout            = ?,  \
              \ server_public_key  = ?,  \
              \ server_private_key = ?,  \
              \ client_public_key  = ?   \
              \ WHERE kpid = ?"

deleteQuery :: Query
deleteQuery = "DELETE FROM keystore WHERE kpid = ?"

getAllKeysQuery :: Query
getAllKeysQuery = "SELECT * FROM keystore"

keyStoreExistsQuery :: Query
keyStoreExistsQuery = "SELECT name FROM sqlite_master                 \
                      \  WHERE type = 'table' AND name = 'keystore'"

createKeyStoreQuery :: Query
createKeyStoreQuery = "CREATE TABLE keystore(                \
                       \ kpid      INTEGER PRIMARY KEY ASC,  \
                       \ timeout   INTEGER NOT NULL,         \
                       \ server_public_key  BLOB NOT NULL,   \
                       \ server_private_key BLOB NOT NULL,   \
                       \ client_public_key  BLOB NOT NULL)"

execQueryInTransaction :: ToRow q => Connection -> Query -> q -> IO ()
execQueryInTransaction conn query params = withTransaction conn $
  execute conn query params

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

addKey :: KeyStoreMVar -> ExpireTime -> PublicKey -> PrivateKey -> PublicKey -> IO KeyParamId
addKey ksm timeout srvPubKey srvPrivKey clntPubKey =
  withMVar ksm $ \KeyStore{..} -> do
    execQueryInTransaction ksConnection addQuery $
      [toField timeout, toField srvPubKey, toField srvPrivKey, toField clntPubKey]

    kpid <- fromIntegral <$> lastInsertRowId ksConnection

    let keyParams = SrvKeyParams kpid timeout srvPrivKey srvPubKey clntPubKey

    modifyIORef ksCache $ HM.insert kpid keyParams

    return kpid


deleteKey :: KeyStoreMVar -> KeyParamId -> IO ()
deleteKey ksm kpid =
  withMVar ksm $ \KeyStore{..} -> do
    execQueryInTransaction ksConnection deleteQuery $ Only kpid

    modifyIORef ksCache $ HM.delete kpid


updateKey :: KeyStoreMVar -> SrvKeyParams -> IO ()
updateKey ksm keyParams@SrvKeyParams{..} =
  withMVar ksm $ \KeyStore{..} -> do
    execQueryInTransaction ksConnection updateQuery $
      [toField skpTimeout, toField skpSrvPubKey, toField skpSrvPrivKey,
       toField skpClnPubKey, toField skpId ]

    modifyIORef ksCache $ HM.insert skpId keyParams


-- Currently we always perform the lookup directly with the in-memory cache
lookupKey :: KeyStoreMVar -> KeyParamId -> IO (Maybe SrvKeyParams)
lookupKey ksm kpid =
  withMVar ksm $ \KeyStore{..} -> do
    cache <- readIORef ksCache
    return $ kpid `HM.lookup` cache


initKeyStore :: FilePath -> IO KeyStoreMVar
initKeyStore fname = do
  conn <- open fname

  withTransaction conn $ do
    ksExists <- keyStoreExists conn

    if (not ksExists)
      then createKeyStore conn
      else checkKeyStoreSchema conn

    keyParams <- query_ conn getAllKeysQuery

    let cache = HM.fromList $ map (\p -> (skpId p, p)) keyParams

    cacheIORef <- newIORef cache

    newMVar $ KeyStore conn cacheIORef

  where
    keyStoreExists :: Connection -> IO Bool
    keyStoreExists conn = do
      r <- query_ conn keyStoreExistsQuery :: IO [Only T.Text]
      return . not . null $ r

    checkKeyStoreSchema :: Connection -> IO ()
    checkKeyStoreSchema conn = do
      [r1,r2,r3,r4,r5] <- query_ conn "PRAGMA table_info(keystore)"
                          :: IO [(Int, T.Text, T.Text, Int, Maybe Null, Int)]

      (0, "kpid", "INTEGER", _, _, 1) <- return r1
      (1, "timeout", "INTEGER", _, _, 0) <- return r2
      (2, "server_public_key", "BLOB", _, _, 0) <- return r3
      (3, "server_private_key", "BLOB", _, _, 0) <- return r4
      (4, "client_public_key", "BLOB", _, _, 0) <- return r5

      return ()

    createKeyStore :: Connection -> IO ()
    createKeyStore conn = do
      execute_ conn createKeyStoreQuery
