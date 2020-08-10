{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Backend
  ( backend
  ) where

import           Common.Route
import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import           Control.Exception (mask_)
import           Control.Lens
import           Control.Monad (forever, join, when)
import           Control.Monad.Catch (finally, try)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (traverse_)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap (runWebSocketsSnap)
import           Obelisk.Backend
import           Obelisk.Route

import qualified Frontend

newtype ClientId =
  ClientId Int
  deriving (Show, Eq, Enum, Ord)

type BoardName = T.Text

data ServerState =
  ServerState
    { _ssClients         :: M.Map ClientId WS.Connection
    , _ssClientIdSeq     :: ClientId
    , _ssFrontendState   :: Frontend.Model
    , _ssPendingSave     :: Bool
    , _ssInactivityTimer :: Maybe ThreadId
    }

makeLenses ''ServerState

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do

      -- an MVar containing a map of state MVars for each board instance
      boardsMVar <- newMVar M.empty :: IO (MVar (M.Map T.Text (MVar ServerState)))

      -- Save the frontend to disk every 60 secs
      _ <- forkIO . forever $ do
        threadDelay 60000000
        modifyMVar_ boardsMVar saveFrontend

      serve $ \case
        BackendRouteMissing :/ _ -> pure ()
        BackendRouteWebSocket :/ boardName -> do
          stateMVar <- liftIO . modifyMVar boardsMVar $ \boardMap ->
            case M.lookup boardName boardMap of
              Nothing -> do
                -- if the board doesn't exist, initialize a new state for it
                mbLoaded <- loadSavedFrontend boardName
                s <- newMVar ServerState
                       { _ssClients         = M.empty
                       , _ssClientIdSeq     = ClientId 0
                       , _ssFrontendState   = fromMaybe Frontend.initModel mbLoaded
                       , _ssPendingSave     = False
                       , _ssInactivityTimer = Nothing
                       }

                pure (M.insert boardName s boardMap, s)

              Just s -> pure (boardMap, s)

          runWebSocketsSnap $ websocketsServer stateMVar

        _ -> pure ()
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

    pure ( s & ssClientIdSeq %~ succ
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
            s & ssPendingSave .~ True
              & ssFrontendState <%~ Frontend.applyEvent ev

      -- send full sync to client on key events
      when (Frontend.isKeyEvent ev) $
         WS.sendTextData
           conn
           (Aeson.encode $ Frontend.mkReplaceEvent newFrontend)

      newState' <-
        if Frontend.isActivityEvent ev
         then setInactivityTimer newState serverState
         else pure newState

      -- send message to other clients
      broadcast newState' (Just clientId) msg

      pure newState'

    Nothing -> pure ()

-- Send a message to all clients, excluding the sender if applicable
broadcast :: ServerState -> Maybe ClientId -> BS.ByteString -> IO ()
broadcast serverState mbSenderId msg =
  let clients = maybe id M.delete mbSenderId $ _ssClients serverState
   in traverse_ (`WS.sendTextData` msg) clients

-- Use filesystem persistence to save the state of the frontend.
saveFrontend :: M.Map BoardName (MVar ServerState) -> IO (M.Map BoardName (MVar ServerState))
saveFrontend boardMap =
  ifor boardMap $ \boardName stateMVar -> modifyMVar stateMVar $ \serverState ->
    if _ssPendingSave serverState
       then do
         Aeson.encodeFile (savedBoardFileName boardName)
           $ _ssFrontendState serverState
         pure (serverState { _ssPendingSave = False }, stateMVar)
       else pure (serverState, stateMVar)

-- Load a saved frontend. Emits nothing if there is no file or decoding fails.
loadSavedFrontend :: BoardName -> IO (Maybe Frontend.Model)
loadSavedFrontend boardName =
  join . either (const Nothing) Just
    <$> try @IO @IOError
            (Aeson.decodeFileStrict $ savedBoardFileName boardName)

savedBoardFileName :: BoardName -> String
savedBoardFileName boardName =
  "saved-state-" <> T.unpack boardName <> ".json"

-- Spawn a thread that will send the inactivity event to all of a board's clients
-- after a timer expires. First kills the current inactivity timer if there is
-- one.
setInactivityTimer :: ServerState -> MVar ServerState -> IO ServerState
setInactivityTimer currentState stateMVar = do
  traverse_ killThread $ _ssInactivityTimer currentState
  tid <- forkIO $ do
    threadDelay 4000000 -- wait before becoming inactive

    let inactivityEv = Frontend.mkInactivityEvent

    mask_ . modifyMVar_ stateMVar $ \s -> do
      broadcast s Nothing . BSL.toStrict
        $ Aeson.encode inactivityEv
      pure $ s & ssFrontendState %~ Frontend.applyEvent inactivityEv
               & ssInactivityTimer .~ Nothing

  pure $ currentState & ssInactivityTimer ?~ tid

