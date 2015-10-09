{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.Map.Strict          as Map
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
            { clientName          :: ClientName
            , clientHandle        :: Handle
            , clientKicked        :: TVar Bool
            , clientMsgChan       :: TChan Message
            , clientBroadcastChan :: TChan Message
            }

data Server = Server
            { serverClientsMap    :: TVar (Map.Map ClientName Client)
            , serverBroadcastChan :: TChan Message
            }

data Message = BroadCast ClientName B.ByteString
             | Tell ClientName B.ByteString
             | Notice B.ByteString
             | Command B.ByteString

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

newClient :: ClientName -> Handle -> TChan Message -> STM Client
newClient name hdl chan = do
  msgChan <- newTChan
  broadcastChan <- dupTChan chan
  kicked <- newTVar False
  return $ Client name hdl kicked msgChan broadcastChan

sendMsgSTM :: Client -> Message -> STM ()
sendMsgSTM Client{..} = writeTChan clientMsgChan

broadcastSTM :: Server -> ClientName -> B.ByteString -> STM ()
broadcastSTM Server{..} name inp = do
  let msg = BroadCast name inp
  writeTChan serverBroadcastChan msg

kickSTM :: Server -> ClientName -> STM ()
kickSTM Server{..} name = do
  clientsMap <- readTVar serverClientsMap
  case Map.lookup name clientsMap of
    Nothing -> return () -- no recipient found, do nothing
    Just client@Client{..} -> writeTVar clientKicked True

notifySTM :: Server -> B.ByteString -> STM ()
notifySTM Server{..} inp = do
  let msg = Notice inp
  writeTChan serverBroadcastChan msg

tellSTM :: Server -> ClientName -> ClientName -> B.ByteString -> STM ()
tellSTM Server{..} fromName toName inp = do
  clientsMap <- readTVar serverClientsMap
  case Map.lookup toName clientsMap of
    Nothing -> return () -- no recipient found, do nothing
    Just client -> sendMsgSTM client $  Tell fromName inp

-------------------------------------------------------------------------------

sendMsg :: Client -> Message -> IO ()
sendMsg client msg = atomically $ sendMsgSTM client msg

notify' :: Server -> B.ByteString -> IO ()
notify' server msg = atomically $ notifySTM server msg

broadcast :: Server -> ClientName -> B.ByteString -> IO ()
broadcast server name inp = atomically $ broadcastSTM server name inp

tell :: Server -> ClientName -> ClientName -> B.ByteString -> IO ()
tell server from to inp = atomically $ tellSTM server from to inp

kick :: Server -> ClientName -> IO ()
kick server name = atomically $ kickSTM server name

removeClient :: Server -> Client -> IO ()
removeClient server@Server{..} Client{..} = atomically $ do
  modifyTVar' serverClientsMap $ \clients -> Map.delete clientName clients
  notifySTM server . B8.pack $ printf "%s has quit." (B8.unpack clientName)

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
  notify' server . B8.pack $ printf "%s has joined." (B8.unpack clientName)
  handlerLoop `race_` receiveInput `race_` receiveBroadCast
  where
    handlerLoop = join . atomically $ do
      kicked <- readTVar clientKicked

      if kicked
        then return $ notify "You have been kicked!"
        else do
          msg <- readTChan clientMsgChan
          return $ do
            continue <- handleMsg msg
            when continue handlerLoop

    handleMsg message = case message of
      Command input ->
        case words (B8.unpack input) of
          ("/quit":_) -> return False
          ("/kick":name:_) -> do
            kick server (B8.pack name)
            return True
          ("/tell":toName:ws) -> do
            tell server clientName (B8.pack toName) (B8.pack $ unwords ws)
            return True
          _ -> do
            broadcast server clientName input
            return True
      Tell name msg -> do
        output name msg
        return True
      _ -> error "invalid message type in the message channel"

    receiveInput = forever $ do
      input <- B.hGetLine clientHandle
      sendMsg client $ Command input

    receiveBroadCast = forever $ join . atomically $
      readTChan clientBroadcastChan >>= \case
        BroadCast name msg -> return $
          unless (name == clientName) $
            output name msg
        Notice msg -> return $ notify msg
        _ -> error "invalid message type in the broadcast channel"

    output name msg =
      B8.hPutStrLn clientHandle . B8.pack $
      printf "%s: %s" (B8.unpack name) (B8.unpack msg)

    notify msg =
      B8.hPutStrLn clientHandle . B8.pack $
      printf "** %s" (B8.unpack msg)

workerThread :: Server -> Handle -> IO ()
workerThread server@Server{..} hdl = mask $ \restore -> do
  client <- getClient
  restore (runClient server client) `finally` removeClient server client
  where
    getClient = do
      B8.hPutStrLn hdl "What is your name?"
      name <- B.hGetLine hdl
      mClient <- atomically $ do
        clients <- readTVar serverClientsMap
        case Map.lookup name clients of
          Just _ -> return Nothing
          Nothing -> do
            client <- newClient name hdl serverBroadcastChan
            writeTVar serverClientsMap $ Map.insert name client clients
            return (Just client)

      case mClient of
        Just client -> do
          B8.hPutStrLn hdl . B8.pack $ printf "You are now known as '%s'." (B8.unpack name)
          return client
        Nothing -> do
          B8.hPutStrLn hdl "The name is already taken. Please choose another one."
          getClient

-- runs the server main loop
runServer :: IO ()
runServer = do
  server <- Server <$> newTVarIO Map.empty <*> newBroadcastTChanIO
  serverSocket <- listenOn (PortNumber 44444)

  forever $ do
    (hdl, host, port) <- accept serverSocket
    printf "%s:%s connected\n" host (show port)

    forkFinally (workerThread server hdl) $ \_ -> hClose hdl

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = runServer
