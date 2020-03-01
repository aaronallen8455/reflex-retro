{-# LANGUAGE OverloadedStrings #-}
module Widget.SimpleButton
  ( simpleButton
  ) where

import qualified Data.Text as T

import           Reflex.Dom.Core

simpleButton :: (DomBuilder t m)
             => T.Text
             -> m (Event t ())
simpleButton label = domEvent Click . fst <$> el' "button" (text label)
