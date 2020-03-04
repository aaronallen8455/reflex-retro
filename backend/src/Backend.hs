{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backend where

import           Common.Route
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad (forever)
import           Control.Monad.Catch (finally)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import           Data.Dependent.Sum
import           Data.Foldable (traverse_)
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap (runWebSocketsSnap)
import           Obelisk.Backend

import qualified Frontend as Frontend

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      clients <- MVar.newMVar [] :: IO (MVar.MVar ServerState)
      clientIdSeq <- MVar.newMVar $ ClientId 0
      curFrontendState <- MVar.newMVar Frontend.initFrontendState
      serve $ \r ->
        case r of
          BackendRoute_Missing  :=> _ -> pure ()
          BackendRoute_WebSocket :=> _ -> do
            runWebSocketsSnap $ websocketsServer clients clientIdSeq curFrontendState
  , _backend_routeEncoder = fullRouteEncoder
  }

newtype ClientId =
  ClientId Int
  deriving (Show, Eq, Enum)

type ServerState = [(ClientId, WS.Connection)]

websocketsServer :: MVar.MVar ServerState -> MVar.MVar ClientId -> MVar.MVar Frontend.FrontendState -> WS.ServerApp
websocketsServer serverState clientIdSeq frontendState pending = do
  clientId <- MVar.modifyMVar clientIdSeq (\x -> pure (succ x, x))
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  MVar.withMVar frontendState $
    WS.sendTextData conn . Aeson.encode . Frontend.mkReplaceEvent

  -- add client to server state
  MVar.modifyMVar_ serverState (pure . ((clientId, conn) :))

  let disconnect = MVar.modifyMVar_ serverState
                                    (pure . filter ((/= clientId) . fst))

  _ <- flip finally disconnect . forever $ do
    -- forward received messages to the other clients
    msg <- WS.receiveData conn

    -- apply the event to the server's frontend model
    case Aeson.decodeStrict msg of
      Just ev -> MVar.modifyMVar_ frontendState $ \fe -> do
        let newFrontend = Frontend.applyFrontendEvent ev fe
        -- send full sync to client on key events
        if Frontend.isKeyEvent ev
           then WS.sendTextData
                  conn
                  (Aeson.encode $ Frontend.mkReplaceEvent newFrontend)
           else pure ()

        pure newFrontend

      Nothing -> pure ()

    broadcast serverState clientId msg

  pure ()

broadcast :: MVar.MVar ServerState -> ClientId -> BS.ByteString -> IO ()
broadcast serverState senderId msg = do
  clients <- map snd . filter ((/= senderId) . fst)
         <$> MVar.readMVar serverState

  traverse_ (flip WS.sendTextData msg) clients

