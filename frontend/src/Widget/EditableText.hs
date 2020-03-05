{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Widget.EditableText
  ( editableTextDynClass
  , editableTextClass
  , editableText
  ) where

import           Control.Monad.Fix (MonadFix)
import qualified Data.Text as T

import           Reflex.Dom.Core

import           Widget.SimpleButton (simpleButton)

data TextBoxEv
  = MakeEditable
  | DoneEditing T.Text
  | CancelEditing

textChanged :: TextBoxEv -> Maybe T.Text
textChanged (DoneEditing txt) = Just txt
textChanged _                 = Nothing

isEditable :: TextBoxEv -> Bool
isEditable MakeEditable  = True
isEditable _             = False

editableTextDynClass :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m, Reflex t)
                     => Dynamic t T.Text
                     -> Dynamic t T.Text
                     -> m (Event t T.Text)
editableTextDynClass txtDyn classDyn = mdo
  isEditableDyn <- foldDyn const False (isEditable <$> textBoxEvents)

  let mkElement editable =
        if editable
           then do
             initVal <- sample $ current txtDyn

             inp <- inputElement
                      def { _inputElementConfig_initialValue = initVal }

             let inpValue = _inputElement_value inp

             confirmEv <- fmap DoneEditing
                        . tagPromptlyDyn inpValue
                      <$> simpleButton "Ok"

             cancelEv <- (CancelEditing <$) <$> simpleButton "Cancel"

             pure $ leftmost [confirmEv, cancelEv]
           else do
             contentEl <- fst <$> elDynClass' "span" classDyn (dynText txtDyn)
             pure $ MakeEditable <$ domEvent Click contentEl

  textBoxEvents <- switchHold never =<< dyn (mkElement <$> isEditableDyn)

  pure $ fmapMaybe textChanged textBoxEvents

editableTextClass :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m, Reflex t)
                  => Dynamic t T.Text
                  -> T.Text
                  -> m (Event t T.Text)
editableTextClass txtDyn classTxt = editableTextDynClass txtDyn (pure classTxt)

editableText :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m, Reflex t)
             => Dynamic t T.Text
             -> m (Event t T.Text)
editableText txtDyn = editableTextClass txtDyn "editable-text"
