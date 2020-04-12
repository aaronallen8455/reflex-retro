{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import           Control.Category
import           Control.Lens
import           Data.Text (Text)
import           Data.Functor.Identity
import           Prelude hiding (id, (.))

import           Obelisk.Route
import           Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_WebSocket :: BackendRoute Text
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Root :: FrontendRoute ()
  FrontendRoute_Board :: FrontendRoute Text
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_WebSocket ->
        PathSegment "websocket" $ singlePathSegmentEncoder
                                . prismEncoder nonEmptySegment
  )
  (\case
      FrontendRoute_Root -> PathEnd $ unitEncoder mempty
      FrontendRoute_Board ->
        PathSegment "board" $ singlePathSegmentEncoder
                            . prismEncoder nonEmptySegment
  )

nonEmptySegment :: Prism' Text Text
nonEmptySegment = prism' id nonEmpty
  where
    nonEmpty "" = Nothing
    nonEmpty t = Just t

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
