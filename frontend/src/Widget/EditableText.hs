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
  | Activity Key

getOutput :: TextBoxEv -> Maybe (Either Key T.Text)
getOutput (DoneEditing txt) = Just $ Right txt
getOutput (Activity kc)     = Just $ Left kc
getOutput _                 = Nothing

isEditable :: TextBoxEv -> Bool
isEditable MakeEditable  = True
isEditable _             = False

isActivity :: TextBoxEv -> Bool
isActivity (Activity _) = True
isActivity _            = False

editableTextDynClass :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m, Reflex t)
                     => Dynamic t T.Text
                     -> Dynamic t T.Text
                     -> m (Event t (Either Key T.Text))
editableTextDynClass txtDyn classDyn = mdo
  isEditableDyn <-
    foldDyn const False
      (isEditable <$> ffilter (not . isActivity) textBoxEvents)

  let mkElement editable =
        if editable
           then do
             initVal <- sample $ current txtDyn

             let editingClass = (<> " editing") <$> classDyn
             elDynClass "div" editingClass $ do
               inp <- inputElement $
                        def & inputElementConfig_initialValue .~ initVal
                            & inputElementConfig_elementConfig
                              . elementConfig_initialAttributes
                                  .~ ("class" =: "text-input")

               let ele = _inputElement_element inp
                   keydownEv = keyCodeLookup . fromIntegral
                           <$> domEvent Keydown ele
                   activityEv = Activity <$> keydownEv
                   enterKeyEv = () <$ ffilter (== Enter) keydownEv
                   inpValue = _inputElement_value inp

               let confirmEv = fmap DoneEditing
                             . tagPromptlyDyn inpValue
                             $ enterKeyEv

               cancelEv <- (CancelEditing <$) <$> simpleButton "Cancel"

               pure $ leftmost [confirmEv, cancelEv, activityEv]
           else do
             contentEl <- fst <$> elDynClass' "span" classDyn (dynText txtDyn)
             pure $ MakeEditable <$ domEvent Click contentEl

  textBoxEvents <- switchHold never =<< dyn (mkElement <$> isEditableDyn)

  pure $ fmapMaybe getOutput textBoxEvents

editableTextClass :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m, Reflex t)
                  => Dynamic t T.Text
                  -> T.Text
                  -> m (Event t (Either Key T.Text))
editableTextClass txtDyn classTxt = editableTextDynClass txtDyn (pure classTxt)

editableText :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m, Reflex t)
             => Dynamic t T.Text
             -> m (Event t (Either Key T.Text))
editableText txtDyn = editableTextClass txtDyn "editable-text"
