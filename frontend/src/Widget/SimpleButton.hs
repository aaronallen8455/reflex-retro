{-# LANGUAGE OverloadedStrings #-}
module Widget.SimpleButton
  ( simpleButton
  , buttonDynClass
  , buttonClass
  ) where

import qualified Data.Text as T

import           Reflex.Dom.Core

buttonDynClass :: (DomBuilder t m, PostBuild t m)
               => Dynamic t T.Text
               -> T.Text
               -> m (Event t ())
buttonDynClass classDyn label =
  domEvent Click . fst <$> elDynClass' "button" classDyn (text label)

buttonClass :: (DomBuilder t m)
            => T.Text
            -> T.Text
            -> m (Event t ())
buttonClass class' label =
  domEvent Click . fst <$> elClass' "button" class' (text label)

simpleButton :: (DomBuilder t m)
             => T.Text
             -> m (Event t ())
simpleButton label = domEvent Click . fst <$> el' "button" (text label)
