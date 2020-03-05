{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module WebSocket
  ( connectWebSocket
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Text.URI as URI

import qualified Language.Javascript.JSaddle.Types as JS
import           Obelisk.Route
import           Reflex.Dom.Core

import           Common.Route
import           Widget.Main (FrontendEvent)

connectWebSocket :: (JS.MonadJSM m, JS.MonadJSM (Performable m), HasJSContext m, PerformEvent t m, TriggerEvent t m, PostBuild t m)
                 => Maybe T.Text -> Event t FrontendEvent -> m (Event t FrontendEvent)
connectWebSocket mbBaseURI outboundEvents = do
  let encoder = either (error . show) id $ checkEncoder fullRouteEncoder

      wsPath = fst . encode encoder
             $ FullRoute_Backend BackendRoute_WebSocket :/ ()

      mbUri = do
        uri' <- URI.mkURI =<< mbBaseURI
        pathPiece <- NE.nonEmpty =<< traverse URI.mkPathPiece wsPath
        wsScheme <- case URI.uriScheme uri' of
                      t | t == URI.mkScheme "https" -> URI.mkScheme "wss"
                        | t == URI.mkScheme "http"  -> URI.mkScheme "ws"
                      _ -> Nothing
        pure $ uri'
          { URI.uriPath   = Just (False, pathPiece)
          , URI.uriScheme = Just wsScheme
          }

  ws <- case mbUri of
          Just uri ->
            webSocket (URI.render uri)
                      def { _webSocketConfig_send =
                              pure . Aeson.encode <$> outboundEvents
                          }
          Nothing -> error "failed to make websocket URI"

  let inboundEvents = fmapMaybe Aeson.decodeStrict
                    $ _webSocket_recv ws

  pure inboundEvents
