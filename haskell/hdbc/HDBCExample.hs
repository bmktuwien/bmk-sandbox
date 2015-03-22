{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Control.Applicative
import Data.Word
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM

type Query = String

type KeyParamId = Int
type ExpireTime = Int

type PublicKey  = B.ByteString
type PrivateKey = B.ByteString

data SrvKeyParams = SrvKeyParams
    { skpId         :: !KeyParamId
    , skpTimeout    :: !ExpireTime -- ^ idle timeout
    , skpSrvPrivKey :: !PrivateKey
    , skpSrvPubKey  :: !PublicKey
    , skpClnPubKey  :: !PublicKey
    }

data KeyStore = KeyStore
  { ksDBConnection :: Connection
  , ksCache        :: HM.HashMap KeyParamId SrvKeyParams
  }

createKeyStoreQuery :: Query
createKeyStoreQuery = "CREATE TABLE keystore( \
                      \  kpid               INTEGER PRIMARY KEY ASC,  \
                      \  timeout            INTEGER,                  \
                      \  server_public_key  BLOB NOT NULL,            \
                      \  server_private_key BLOB NOT NULL,            \
                      \  client_public_key  BLOB NOT NULL)"

addQuery :: Query
addQuery = "INSERT INTO keystore                                                   \
            \  (timeout, server_public_key, server_private_key, client_public_key) \
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

getAllQuery :: Query
getAllQuery = "SELECT * FROM keystore"


execQueryInTransaction :: IConnection conn => conn -> Query -> [SqlValue] -> IO ()
execQueryInTransaction conn query params = withTransaction conn $ \conn' ->
  void $ run conn' query params


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


createKeyStore :: FilePath -> IO ()
createKeyStore fname0 = do
  conn <- connectSqlite3 fname0
  execQueryInTransaction conn createKeyStoreQuery []


add :: KeyStore -> ExpireTime -> PublicKey -> PrivateKey -> PublicKey -> IO ()
add KeyStore{..} timeout srvPubKey srvPrivKey clntPubKey =
  execQueryInTransaction ksDBConnection addQuery $
  [toSql timeout, toSql srvPubKey, toSql srvPrivKey, toSql clntPubKey]


update :: KeyStore -> ExpireTime -> PublicKey -> PrivateKey -> PublicKey -> IO ()
update KeyStore {..} timeout srvPubKey srvPrivKey clntPubKey =
  execQueryInTransaction ksDBConnection updateQuery $
  [toSql timeout, toSql srvPubKey, toSql srvPrivKey, toSql clntPubKey]


delete :: KeyStore -> KeyParamId -> IO ()
delete KeyStore {..} kpid =
  execQueryInTransaction ksDBConnection deleteQuery [toSql kpid]

initKeyStore :: FilePath -> IO KeyStore
initKeyStore fname0 = do
  conn <- connectSqlite3 fname0
  cache <- HM.fromList . map convRow <$> quickQuery' conn getAllQuery []

  return $ KeyStore conn cache

  where
    convRow :: [SqlValue] -> (KeyParamId, SrvKeyParams)
    convRow [timeout, kpid, srvPubKey, srvPrivKey, clntPubKey] =
      (fromSql kpid, SrvKeyParams (fromSql kpid) (fromSql timeout)
      (fromSql srvPubKey) (fromSql srvPrivKey) (fromSql clntPubKey))


main :: IO ()
main = do
  createKeyStore "keystore.db"

  keyStore <- initKeyStore "keystore.db"

  add keyStore 300 "deadbeef" "deadbeef" "deadbeef"
