{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Widget.EditableText
  ( editableText
  ) where

import           Control.Monad.Fix (MonadFix)
import qualified Data.Text as T

import           Reflex.Dom.Core

data TextBoxEv
  = MakeEditable
  | DoneEditing T.Text
  | CancelEditing

textChanged :: TextBoxEv -> Maybe T.Text
textChanged (DoneEditing txt) = Just txt
textChanged _                 = Nothing

isEditable :: TextBoxEv -> Maybe Bool
isEditable MakeEditable  = Just True
isEditable _             = Just False

editableText :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m, Reflex t)
             => Dynamic t T.Text
             -> m (Event t T.Text)
editableText txtDyn = mdo
  isEditableDyn <- foldDyn const False (fmapMaybe isEditable textBoxEvents)

  let mkElement isEditable =
        if isEditable
           then do
             initVal <- sample $ current txtDyn
             inp <- inputElement def { _inputElementConfig_initialValue = initVal }
             let inpValue = _inputElement_value inp

             confirmEv <- fmap DoneEditing
                        . tagPromptlyDyn inpValue
                      <$> button "Ok"

             cancelEv <- (CancelEditing <$) <$> button "Cancel"
             pure $ leftmost [confirmEv, cancelEv]
           else do
             contentEl <- fst <$> elClass' "span" "editable-text" (dynText txtDyn)
             pure $ MakeEditable <$ domEvent Click contentEl

  textBoxEvents <- switchHold never =<< dyn (mkElement <$> isEditableDyn)

  pure $ fmapMaybe textChanged textBoxEvents
