{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Widget.ActionItems
  ( ActionItemState
  , ActionItemEvent
  , applyActionItemEvent
  , actionItemsWidget
  ) where

import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import qualified Data.Aeson.TH as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import           Safe (headMay)

import           Reflex.Dom.Core

import           Widget.EditableText (editableTextDynClass)

data ActionItemState =
  ActionItemState
    { _aiContent   :: T.Text
    , _aiCompleted :: Bool
    } deriving (Show, Eq)

makeLenses ''ActionItemState
Aeson.deriveJSON Aeson.defaultOptions ''ActionItemState

data ActionItemEvent
  = NewActionItem T.Text
  | ContentChange Int T.Text
  | DeleteActionItem Int
  | ChangeCompleted Int Bool

Aeson.deriveJSON Aeson.defaultOptions ''ActionItemEvent

applyActionItemEvent :: ActionItemEvent
                     -> M.Map Int ActionItemState
                     -> M.Map Int ActionItemState
applyActionItemEvent (NewActionItem txt) m
  | not $ T.null txt =
    let idx = maybe 0 (succ . fst) $ M.lookupMax m
     in M.insert idx (ActionItemState txt False) m
  | otherwise = m
applyActionItemEvent (ContentChange i txt) m
  | not $ T.null txt =
    M.adjust (aiContent .~ txt) i m
  | otherwise = m
applyActionItemEvent (DeleteActionItem i) m =
  M.delete i m
applyActionItemEvent (ChangeCompleted i b) m =
  M.adjust (aiCompleted .~ b) i m

actionItemsWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
                  => Dynamic t (M.Map Int ActionItemState) -> m (Event t ActionItemEvent)
actionItemsWidget aiStatesDyn = divClass "action-items" $ do
  el "h3" $ text "Action Items"

  addActionItemDyn <- _inputElement_value <$> inputElement def

  addActionItemClickEv <- domEvent Click . fst
                      <$> el' "button" (text "Add Action Item")

  let addActionItemEv = NewActionItem <$> current addActionItemDyn
                                      <@  addActionItemClickEv

  actionItemsEv
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey aiStatesDyn actionItemWidget

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

  deletedEv <- (DeleteActionItem aiId <$) . domEvent Click . fst
           <$> el' "button" (text "X")

  pure $ leftmost [contentChangedEv, completedEv, deletedEv]

