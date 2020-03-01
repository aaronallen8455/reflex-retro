{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend where

import           Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Obelisk.Configs as Cfg
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Generated.Static
import qualified Text.URI as URI

import           Reflex.Dom.Core

import           Common.Route
import qualified Widget.ActionItems as ActionItems
import qualified Widget.Columns as Columns
import           Widget.EditableText (editableText)

data FrontendState =
  FrontendState
    { _fsColumns     :: M.Map Int Columns.ColumnState
    , _fsActionItems :: M.Map Int ActionItems.ActionItemState
    , _fsTitle       :: T.Text
    }

makeLenses ''FrontendState
Aeson.deriveJSON Aeson.defaultOptions ''FrontendState

initFrontendState :: FrontendState
initFrontendState = FrontendState Columns.initColumns M.empty "Retro"

data FrontendEvent
  = ColEvent Columns.ColumnEvent
  | ActionItemEvent ActionItems.ActionItemEvent
  | Replace FrontendState
  | ChangeTitle T.Text

Aeson.deriveJSON Aeson.defaultOptions ''FrontendEvent

applyFrontendEvent :: FrontendEvent -> FrontendState -> FrontendState
applyFrontendEvent (Replace fs) _ = fs
applyFrontendEvent (ColEvent ev) fs =
  fs & fsColumns %~ Columns.applyColumnEvent ev
applyFrontendEvent (ActionItemEvent ev) fs =
  fs & fsActionItems %~ ActionItems.applyActionItemEvent ev
applyFrontendEvent (ChangeTitle txt) fs
  | not $ T.null txt = fs & fsTitle .~ txt
  | otherwise = fs

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Reflex Retro"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank

  , _frontend_body = do
      mbBaseURI <- Cfg.getTextConfig "common/route"
      _ <- prerender (text "Loading...") $ mdo
        let encoder = either (error . show) id $ checkEncoder fullRouteEncoder
            wsPath = fst $ encode encoder $ FullRoute_Backend BackendRoute_WebSocket :/ ()
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

        -- will changes applied by the messages received be sent back to the server?
        ws <- case mbUri of
                Just uri ->
                  webSocket (URI.render uri) def { _webSocketConfig_send = pure . Aeson.encode <$> columnEvents }
                Nothing -> error "failed to make websocket URI"

        let wsEvents = fmapMaybe Aeson.decodeStrict
                     $ _webSocket_recv ws

            frontendEvents = leftmost [ wsEvents
                                      , editTitleEv
                                      , columnEvents
                                      , actionItemEvents
                                      ]

        stateDyn <- foldDyn applyFrontendEvent initFrontendState
                    frontendEvents

        editTitleEv
          <- fmap ChangeTitle
         <$> elClass "h2" "title" (editableText (_fsTitle <$> stateDyn))

        columnEvents <- fmap ColEvent
                    <$> Columns.columnsWidget (_fsColumns <$> stateDyn)

        actionItemEvents <- fmap ActionItemEvent
                        <$> ActionItems.actionItemsWidget (_fsActionItems <$> stateDyn)

        pure ()

      pure ()
  }
