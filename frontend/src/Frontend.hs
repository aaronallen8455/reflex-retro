{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend where

import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Generated.Static

import           Reflex.Dom.Core

import           Common.Route
import qualified Widget.Columns as Columns

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Reflex Retro"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank

  , _frontend_body = mdo

      columnMapDyn <- foldDyn Columns.applyColumnEvent Columns.initColumns
                      columnEvents

      columnEvents <- Columns.columnsWidget columnMapDyn

      pure ()
  }

