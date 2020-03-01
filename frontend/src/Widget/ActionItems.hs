{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Widget.ActionItems
  ( ActionItemState
  , ActionItemEvent
  , ActionItems
  , applyActionItemEvent
  , actionItemsWidget
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
import           Widget.EditableText (editableTextDynClass)
import           Widget.SimpleButton (simpleButton)

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
                     then "**(Completed)** "
                     else ""

data ActionItemEvent
  = NewActionItem T.Text
  | ContentChange Int T.Text
  | DeleteActionItem Int
  | ChangeCompleted Int Bool

Aeson.deriveJSON Aeson.defaultOptions ''ActionItemEvent

newtype ActionItems =
  ActionItems
    { getActionItems :: M.Map Int ActionItemState
    } deriving (Show, Eq, Aeson.FromJSON, Aeson.ToJSON, Semigroup, Monoid)

instance ToMarkdown ActionItems where
  toMarkdown (ActionItems actionItemMap) =
    T.unlines $
      [ "### Action Items"
      , ""
      ] <> map (\(n, i) -> T.pack (show n) <> ". " <> toMarkdown i)
               ([1 :: Int ..] `zip` M.elems actionItemMap)

applyActionItemEvent :: ActionItemEvent
                     -> ActionItems
                     -> ActionItems
applyActionItemEvent (NewActionItem txt) ai@(ActionItems m)
  | not $ T.null txt =
    let idx = maybe 0 (succ . fst) $ M.lookupMax m
     in ActionItems $ M.insert idx (ActionItemState txt False) m
  | otherwise = ai
applyActionItemEvent (ContentChange i txt) ai@(ActionItems m)
  | not $ T.null txt =
    ActionItems $ M.adjust (aiContent .~ txt) i m
  | otherwise = ai
applyActionItemEvent (DeleteActionItem i) (ActionItems m) =
  ActionItems $ M.delete i m
applyActionItemEvent (ChangeCompleted i b) (ActionItems m) =
  ActionItems $ M.adjust (aiCompleted .~ b) i m

actionItemsWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
                  => Dynamic t ActionItems -> m (Event t ActionItemEvent)
actionItemsWidget aiStatesDyn = divClass "action-items" $ do
  el "h3" $ text "Action Items"

  addActionItemDyn <- _inputElement_value <$> inputElement def

  addActionItemClickEv <- simpleButton "Add Action Item"

  let addActionItemEv = NewActionItem <$> current addActionItemDyn
                                      <@  addActionItemClickEv

  actionItemsEv
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey (getActionItems <$> aiStatesDyn) actionItemWidget

  pure $ leftmost [addActionItemEv, actionItemsEv]

actionItemWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
                 => Int -> Dynamic t ActionItemState -> m (Event t ActionItemEvent)
actionItemWidget aiId aiStateDyn = divClass "action-item" $ do
  initChecked <- sample . current $ _aiCompleted <$> aiStateDyn
  completedCheckBox <- inputElement $ def
    { _inputElementConfig_setChecked = Just . updated $ _aiCompleted <$> aiStateDyn
    , _inputElementConfig_initialChecked = initChecked
    } & inputElementConfig_elementConfig . elementConfig_initialAttributes
          .~ "type" =: "checkbox"

  let completedClass True = "completed-action-item"
      completedClass _    = ""
      completedEv = ChangeCompleted aiId
                <$> _inputElement_checkedChange completedCheckBox

  contentChangedEv
    <- fmap (ContentChange aiId)
   <$> editableTextDynClass (_aiContent <$> aiStateDyn)
                            (completedClass . _aiCompleted <$> aiStateDyn)

  deletedEv <- (DeleteActionItem aiId <$) <$> simpleButton "X"

  pure $ leftmost [contentChangedEv, completedEv, deletedEv]

