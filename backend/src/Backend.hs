{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Backend
  ( backend
  ) where

import           Common.Route
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import           Control.Lens
import           Control.Monad (forever, join)
import           Control.Monad.Catch (finally, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import           Data.Dependent.Sum
import           Data.Foldable (traverse_)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap (runWebSocketsSnap)
import           Obelisk.Backend

import qualified Frontend as Frontend

newtype ClientId =
  ClientId Int
  deriving (Show, Eq, Enum, Ord)

data ServerState =
  ServerState
    { _ssClients       :: M.Map ClientId WS.Connection
    , _ssClientIdSeq   :: ClientId
    , _ssFrontendState :: Frontend.Model
    , _ssPendingSave   :: Bool
    }

makeLenses ''ServerState

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      mbLoaded <- loadSavedFrontend

      stateMVar
        <- newMVar ServerState
             { _ssClients       = M.empty
             , _ssClientIdSeq   = ClientId 0
             , _ssFrontendState = fromMaybe Frontend.initModel mbLoaded
             , _ssPendingSave   = False
             }

      -- Save the frontend to disk every 60 secs
      _ <- forkIO . forever $ do
        threadDelay 60000000
        modifyMVar_ stateMVar saveFrontend

      serve $ \r ->
        case r of
          BackendRoute_Missing :=> _   -> pure ()
          BackendRoute_WebSocket :=> _ -> do
            runWebSocketsSnap $ websocketsServer stateMVar
  , _backend_routeEncoder = fullRouteEncoder
  }

websocketsServer :: MVar ServerState -> WS.ServerApp
websocketsServer serverState pending = do
  (clientId, conn) <- addClient serverState pending

  let disconnect = modifyMVar_ serverState
                               (pure . (ssClients . at clientId .~ Nothing))

  _ <- flip finally disconnect . forever
     $ talkToClient serverState clientId conn

  pure ()

addClient :: MVar ServerState -> WS.PendingConnection -> IO (ClientId, WS.Connection)
addClient serverState pending = do
  modifyMVar serverState $ \s -> do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    WS.sendTextData conn . Aeson.encode
                         . Frontend.mkReplaceEvent
                         $ _ssFrontendState s

    let clientId = _ssClientIdSeq s

    pure $ ( s & ssClientIdSeq %~ succ
               & ssClients . at clientId ?~ conn
           , (clientId, conn)
           )

talkToClient :: MVar ServerState -> ClientId -> WS.Connection -> IO ()
talkToClient serverState clientId conn = do
  -- forward received messages to the other clients
  msg <- WS.receiveData conn

  -- apply the event to the server's frontend model
  case Aeson.decodeStrict msg of
    Just ev -> modifyMVar_ serverState $ \s -> do
      let (newFrontend, newState) =
            s & ssPendingSave %~ not
              & ssFrontendState <%~ Frontend.applyEvent ev

      -- send full sync to client on key events
      if Frontend.isKeyEvent ev
         then WS.sendTextData
                conn
                (Aeson.encode $ Frontend.mkReplaceEvent newFrontend)
         else pure ()

      -- send message to other clients
      broadcast newState clientId msg

      pure newState

    Nothing -> pure ()

broadcast :: ServerState -> ClientId -> BS.ByteString -> IO ()
broadcast serverState senderId msg =
  let clients = M.delete senderId $ _ssClients serverState
   in traverse_ (flip WS.sendTextData msg) clients

-- Use filesystem persistence to save the state of the frontend.
saveFrontend :: ServerState -> IO ServerState
saveFrontend serverState = do
  if _ssPendingSave serverState
     then do
       Aeson.encodeFile "saved-state.json" $ _ssFrontendState serverState
       pure serverState { _ssPendingSave = False }
     else pure serverState

-- Load a saved frontend. Emits nothing if there is no file or decoding fails.
loadSavedFrontend :: IO (Maybe Frontend.Model)
loadSavedFrontend = do
  join . either (const Nothing) Just
    <$> try @IO @IOError (Aeson.decodeFileStrict "saved-state.json")

