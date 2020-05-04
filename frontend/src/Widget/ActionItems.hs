{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
module Widget.ActionItems
  ( Ev
  , Model
  , applyEvent
  , widget
  , isKeyEvent
  ) where

import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import           Safe (headMay)

import           Reflex.Dom.Core

import           Common.Markdown (ToMarkdown(..))
import           Widget.ConfirmDialog (confirmDialog)
import           Widget.EditableText (editableTextDynClass)
import           Widget.SimpleButton (buttonClass, buttonDynAttr)
import           Widget.TextEntry (textEntry)

data ActionItemState =
  ActionItemState
    { _aiContent   :: T.Text
    , _aiCompleted :: Bool
    } deriving (Show, Eq)

makeLenses ''ActionItemState
Aeson.deriveJSON Aeson.defaultOptions ''ActionItemState

instance ToMarkdown ActionItemState where
  toMarkdown ai = completed <> _aiContent ai
    where
      completed = if _aiCompleted ai
                     then "**(Done)** "
                     else ""

data Ev
  = NewActionItem T.Text
  | ContentChange Int T.Text
  | DeleteActionItem Int
  | ChangeCompleted Int Bool
  | DeleteCompleted

Aeson.deriveJSON Aeson.defaultOptions ''Ev

newtype Model =
  Model
    { getActionItems :: M.Map Int ActionItemState
    } deriving (Show, Eq, Aeson.FromJSON, Aeson.ToJSON, Semigroup, Monoid)

instance ToMarkdown Model where
  toMarkdown (Model actionItemMap) =
    T.unlines $
      "### Action Items"
      : map (\(n, i) -> T.pack (show n) <> ". " <> toMarkdown i)
            ([1 :: Int ..] `zip` M.elems actionItemMap)

applyEvent :: Ev
           -> Model
           -> Model
applyEvent (NewActionItem txt) ai@(Model m)
  | not $ T.null txt =
    let idx = maybe 0 (succ . fst) $ M.lookupMax m
     in Model $ M.insert idx (ActionItemState txt False) m
  | otherwise = ai
applyEvent (ContentChange i txt) ai@(Model m)
  | not $ T.null txt =
    Model $ M.adjust (aiContent .~ txt) i m
  | otherwise = ai
applyEvent (DeleteActionItem i) (Model m) =
  Model $ M.delete i m
applyEvent (ChangeCompleted i b) (Model m) =
  Model $ M.adjust (aiCompleted .~ b) i m
applyEvent DeleteCompleted (Model m) =
  Model $ M.filter (not . _aiCompleted) m

isKeyEvent :: Ev -> Bool
isKeyEvent (NewActionItem _) = True
isKeyEvent DeleteCompleted = True
isKeyEvent _ = False

widget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
       => Dynamic t Model -> m (Event t Ev)
widget modelDyn = divClass "action-items" $ mdo
  el "h3" $ text "Action Items"

  deleteCompletedEv <- deleteCompletedButton modelDyn

  addActionItemEv <- fmap NewActionItem
                 <$> textEntry "Add Action Item..."

  actionItemsEv
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey (getActionItems <$> modelDyn) actionItemWidget

  pure $ leftmost [addActionItemEv, actionItemsEv, deleteCompletedEv]

actionItemWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
                 => Int -> Dynamic t ActionItemState -> m (Event t Ev)
actionItemWidget aiId aiStateDyn = divClass "action-item" $ do
  initChecked <- sample . current $ _aiCompleted <$> aiStateDyn
  completedCheckBox <- inputElement $ def
    { _inputElementConfig_setChecked = Just . updated $ _aiCompleted <$> aiStateDyn
    , _inputElementConfig_initialChecked = initChecked
    } & inputElementConfig_elementConfig . elementConfig_initialAttributes
          .~ "type" =: "checkbox"

  let completedClass True = "completed-item"
      completedClass _    = "item"
      completedEv = ChangeCompleted aiId
                <$> _inputElement_checkedChange completedCheckBox

  contentChangedEv
    <- fmap (ContentChange aiId)
   <$> editableTextDynClass (_aiContent <$> aiStateDyn)
                            (completedClass . _aiCompleted <$> aiStateDyn)

  deletedEv <- (DeleteActionItem aiId <$) <$> buttonClass "delete-button" "×"

  pure $ leftmost [contentChangedEv, completedEv, deletedEv]

deleteCompletedButton :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
                      => Dynamic t Model
                      -> m (Event t Ev)
deleteCompletedButton modelDyn = do
  let disabledAttr (Model m) =
        if any _aiCompleted m
           then mempty
           else "disabled" =: ""

      attrsDyn = ffor modelDyn $ \m ->
           "class" =: "fancy-button delete-completed-button"
        <> disabledAttr m

  deleteCompletedClickEv <- buttonDynAttr attrsDyn "Delete Completed Items"

  (DeleteCompleted <$) . ffilter id
    <$> confirmDialog
          "Delete Completed Items"
          (pure "Are you sure you want to delete all the completed action items?")
          deleteCompletedClickEv


