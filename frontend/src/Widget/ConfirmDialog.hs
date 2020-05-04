{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Widget.ConfirmDialog
  ( confirmDialog
  ) where

import           Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import           Reflex.Dom.Core

import           Widget.SimpleButton (buttonClass)

confirmDialog :: (MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
              => T.Text -> Dynamic t T.Text -> Event t () -> m (Event t Bool)
confirmDialog title messageDyn openEv = mdo
  let notOpen = never <$ blank
      openCloseEv =
        leftmost [ notOpen <$ optionSelectedEv
                 , dialogWidget title messageDyn <$ openEv
                 ]

  optionSelectedEv <- switchDyn <$> widgetHold notOpen openCloseEv

  pure optionSelectedEv

dialogWidget :: (DomBuilder t m, PostBuild t m)
             => T.Text -> Dynamic t T.Text -> m (Event t Bool)
dialogWidget title messageDyn = do
  elClass "div" "modal-overlay" blank
  elClass "div" "modal" $ do
    el "h3" $ text title
    el "p" $ dynText messageDyn

    (confirmEv, cancelEv)
      <- elClass "div" "modal-button-wrapper"
       $ (,)
     <$> buttonClass "fancy-button" "Proceed"
     <*> buttonClass "fancy-button" "Cancel"

    pure $ leftmost [ True <$ confirmEv
                    , False <$ cancelEv
                    ]
