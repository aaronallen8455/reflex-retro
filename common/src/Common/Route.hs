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
  BackendRouteMissing :: BackendRoute ()
  BackendRouteWebSocket :: BackendRoute Text
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRouteRoot :: FrontendRoute ()
  FrontendRouteBoard :: FrontendRoute Text
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRouteMissing :/ ())
  (\case
      BackendRouteMissing -> PathSegment "missing" $ unitEncoder mempty
      BackendRouteWebSocket ->
        PathSegment "websocket" $ singlePathSegmentEncoder
                                . prismEncoder nonEmptySegment
  )
  (\case
      FrontendRouteRoot -> PathEnd $ unitEncoder mempty
      FrontendRouteBoard ->
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
