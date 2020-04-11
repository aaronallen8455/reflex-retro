{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Frontend
  ( frontend
  , module Main
  ) where

import qualified Obelisk.Configs as Cfg
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Obelisk.Generated.Static
import           Reflex.Dom.Core

import           Common.Route
import           WebSocket (connectWebSocket)
import           Widget.Main as Main

import           Debug.Trace

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Reflex Retro"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank

  , _frontend_body = subRoute_ $ \case
        FrontendRoute_Main -> do
          (boardName, _) <- sample =<< current <$> askRoute
          traceShow boardName pure ()

          mbBaseURI <- Cfg.getTextConfig "common/route"

          _ <- prerender (text "Loading...") $ mdo

            wsEvents <- connectWebSocket mbBaseURI boardName mainEvents

            modelDyn <- foldDyn Main.applyEvents
                                Main.initModel
                                frontendEvents

            mainEvents <- Main.widget modelDyn

            let frontendEvents = mergeList [mainEvents, wsEvents]

            pure ()

          pure ()
  }
