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
import           Widget.EditableText (editableTextDynClass)
import           Widget.SimpleButton (buttonClass)
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

isKeyEvent :: Ev -> Bool
isKeyEvent (NewActionItem _) = True
isKeyEvent _ = False

widget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
       => Dynamic t Model -> m (Event t Ev)
widget modelDyn = divClass "action-items" $ mdo
  el "h3" $ text "Action Items"

  addActionItemEv <- fmap NewActionItem
                 <$> textEntry "Add Action Item..."

  actionItemsEv
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey (getActionItems <$> modelDyn) actionItemWidget

  pure $ leftmost [addActionItemEv, actionItemsEv]

actionItemWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
                 => Int -> Dynamic t ActionItemState -> m (Event t Ev)
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

  deletedEv <- (DeleteActionItem aiId <$) <$> buttonClass "delete-button" "×"

  pure $ leftmost [contentChangedEv, completedEv, deletedEv]



