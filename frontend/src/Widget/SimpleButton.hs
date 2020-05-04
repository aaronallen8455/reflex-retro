{-# LANGUAGE OverloadedStrings #-}
module Widget.SimpleButton
  ( simpleButton
  , buttonDynAttr
  , buttonDynClass
  , buttonClass
  ) where

import qualified Data.Map as M
import qualified Data.Text as T

import           Reflex.Dom.Core

buttonDynAttr :: (DomBuilder t m, PostBuild t m)
              => Dynamic t (M.Map T.Text T.Text)
              -> T.Text
              -> m (Event t ())
buttonDynAttr attrDyn label =
  domEvent Click . fst <$> elDynAttr' "button" attrDyn (text label)

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
