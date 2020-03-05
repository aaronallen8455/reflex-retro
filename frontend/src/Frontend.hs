{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend
  ( frontend
  , module Main
  ) where

import qualified Obelisk.Configs as Cfg
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Generated.Static
import           Reflex.Dom.Core

import           Common.Route
import           WebSocket (connectWebSocket)
import           Widget.Main as Main

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Reflex Retro"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank

  , _frontend_body = do
      mbBaseURI <- Cfg.getTextConfig "common/route"

      _ <- prerender (text "Loading...") $ mdo

        wsEvents <- connectWebSocket mbBaseURI mainEvents

        stateDyn <- foldDyn Main.applyFrontendEvents
                            Main.initFrontendState
                            frontendEvents

        mainEvents <- Main.frontendWidget stateDyn

        let frontendEvents = mergeList [mainEvents, wsEvents]

        pure ()

      pure ()
  }
