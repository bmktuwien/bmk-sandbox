{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.Map.Strict              as Map
import           GHC.IO.Handle
import           Network
import           Text.Printf

-------------------------------------------------------------------------------
--------------------------------1-----------------------------------------------

-- avaiable commands

-- * /tell name message (Sends 'message' to the user 'name')
-- * /kick name         (Disconnects user 'name')
-- * /quit              (Disconnects the current client)
-- * message            (Any other message is broadcast as a message to
--                       the connected clients)

type ClientName = B.ByteString

data Client = Client
            { clientName   :: ClientName
            , clientHandle :: Handle
            , clientChan   :: TChan Message
            }

data Server = Server
            { serverClientsMap :: TVar (Map.Map ClientName Client)
            }

data Message = BroadCast ClientName B.ByteString
             | Tell ClientName B.ByteString

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  chan <- newTChan
  return $ Client name handle chan

sendMsg :: Client -> Message -> STM ()
sendMsg Client{..} = writeTChan clientChan

broadCast :: Server -> ClientName -> B.ByteString -> IO ()
broadCast Server{..} name inp = atomically $ do
  let msg = BroadCast name inp
  clients <- readTVar serverClientsMap
  void $ forM clients $ \client -> sendMsg client msg

removeClient :: Server -> Client -> IO ()
removeClient Server{..} Client{..} = atomically $
  modifyTVar' serverClientsMap $ \clients -> Map.delete clientName clients

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} =
  race_ handleMsg receiveInput
  where
    handleMsg = forever $ join . atomically $ do
      msg <- readTChan clientChan
      case msg of
        BroadCast name msg -> return (output name msg)

    receiveInput = forever $ do
      input <- B.hGetLine clientHandle
      broadCast server clientName input

    output name msg =
      B8.hPutStrLn clientHandle . B8.pack $
      printf "%s: %s" (B8.unpack name) (B8.unpack msg)

workerThread :: Server -> Handle -> IO ()
workerThread server@Server{..} hdl = mask $ \restore -> do
  client <- getClient hdl
  restore (runClient server client) `finally` removeClient server client
  where
    getClient hdl = do
      B8.hPutStrLn hdl "What is your name?"
      name <- B.hGetLine hdl
      mClient <- atomically $ do
        clients <- readTVar serverClientsMap
        case Map.lookup name clients of
          Just _ -> return Nothing
          Nothing -> do
            client <- newClient name hdl
            writeTVar serverClientsMap $ Map.insert name client clients
            return (Just client)

      case mClient of
        Just client -> do
          B8.hPutStrLn hdl . B8.pack $ printf "You are now known as '%s'." (B8.unpack name)
          return client
        Nothing -> do
          B8.hPutStrLn hdl "The name is already taken. Please choose another one."
          getClient hdl

-- runs the server main loop
runServer :: IO ()
runServer = do
  server <- Server <$> newTVarIO Map.empty
  serverSocket <- listenOn (PortNumber 44444)

  forever $ do
    (hdl, host, port) <- accept serverSocket
    printf "%s:%s connected\n" host (show port)

    forkFinally (workerThread server hdl) $ \_ -> hClose hdl

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = runServer
