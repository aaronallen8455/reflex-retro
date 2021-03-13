{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Widget.Columns
  ( Model
  , Ev
  , widget
  , applyEvent
  , initColumns
  , isKeyEvent
  , isActivityEvent
  ) where

import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import qualified Data.Aeson.TH as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import           Safe (headMay)

import           Reflex.Dom.Core

import           Common.Markdown (ToMarkdown(..))
import qualified Widget.Cards as Cards
import           Widget.ConfirmDialog (confirmDialog)
import           Widget.EditableText (editableText)
import           Widget.SimpleButton (buttonClass, buttonDynAttr)
import           Widget.TextEntry (textEntry)

data Model =
  Model
    { _colTitle :: T.Text
    , _colCards :: M.Map Int Cards.Model
    }

makeLenses ''Model
Aeson.deriveJSON Aeson.defaultOptions ''Model

instance ToMarkdown Model where
  toMarkdown cs =
    T.unlines
      $ "### " <> _colTitle cs
      : zipWith (\i c -> T.pack (show i) <> ". " <> toMarkdown c)
                [1 :: Int ..]
                (M.elems (_colCards cs))

data Ev
  = DeleteColumn Int
  | AddColumn T.Text
  | ColCardEvent Int Cards.Ev
  | ChangeTitle Int T.Text
  | DeleteAllCards
  | Activity

Aeson.deriveJSON Aeson.defaultOptions ''Ev

applyEvent :: Ev -> M.Map Int Model -> M.Map Int Model
applyEvent (AddColumn colName) colMap
  | T.null colName = colMap
  | otherwise =
    let nxtId = maybe 0 (succ . fst) $ M.lookupMax colMap
     in M.insert nxtId (Model colName M.empty) colMap
applyEvent (DeleteColumn colId) colMap
  = M.delete colId colMap
applyEvent (ColCardEvent colId cardEvent) colMap
  = M.adjust (colCards %~ Cards.applyEvent cardEvent) colId colMap
applyEvent (ChangeTitle colId newTitle) colMap
  | T.null newTitle = colMap
  | otherwise = M.adjust (colTitle .~ newTitle) colId colMap
applyEvent DeleteAllCards colMap
  = colMap & mapped . colCards .~ M.empty
applyEvent Activity colMap = colMap

isKeyEvent :: Ev -> Bool
isKeyEvent (AddColumn _) = True
isKeyEvent (ColCardEvent _ ev) = Cards.isKeyEvent ev
isKeyEvent DeleteAllCards = True
isKeyEvent _ = False

isActivityEvent :: Ev -> Bool
isActivityEvent Activity = True
isActivityEvent (ColCardEvent _ e) = Cards.isActivityEvent e
isActivityEvent _ = False

widget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
       => Dynamic t (M.Map Int Model) -> m () -> m (Event t Ev)
widget colMapDyn activityMonitor = do
  (addColumnEv, deleteAllCardsEv)
    <- elClass "div" "col-action-wrapper" $
      (,) <$> (fmap . fmap) (either (const Activity) AddColumn)
                            (textEntry "Add Column...")
          <*> (activityMonitor *> deleteAllCardsButton colMapDyn)

  columnWidgetEvents
    <- elClass "div" "columns"
     $ flip fforMaybe (headMay . M.elems)
   <$> listViewWithKey colMapDyn columnWidget

  pure $ leftmost [addColumnEv, columnWidgetEvents, deleteAllCardsEv]

columnWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
             => Int -> Dynamic t Model -> m (Event t Ev)
columnWidget colId modelDyn = elClass "div" "column" $ do
  changeTitleEv
    <- (fmap . fmap) (either (const Activity) (ChangeTitle colId))
     . el "h3" $ editableText (_colTitle <$> modelDyn)

  deleteClickEv <- buttonClass "delete-button" "×"

  let deleteDialogMessage = ffor modelDyn $ \m ->
        "Are you sure you want to delete the column '" <> _colTitle m <> "'?"

  deleteColumnEv
    <- (DeleteColumn colId <$)
     . ffilter id
   <$> confirmDialog "Delete Column" deleteDialogMessage deleteClickEv

  cardWidgetEvents <- Cards.widget $ _colCards <$> modelDyn

  let cardEvents = ColCardEvent colId <$> cardWidgetEvents

  pure $ leftmost [ cardEvents
                  , deleteColumnEv
                  , changeTitleEv
                  ]

deleteAllCardsButton :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
                     => Dynamic t (M.Map Int Model) -> m (Event t Ev)
deleteAllCardsButton modelDyn = do
  let disabledAttr m =
        if all (M.null . _colCards) m
           then "disabled" =: ""
           else mempty

      attrsDyn = ffor modelDyn $ \m ->
           "class" =: "fancy-button delete-all-button"
        <> disabledAttr m

  deleteAllClickEv <- buttonDynAttr attrsDyn "Delete All Cards"

  (DeleteAllCards <$) . ffilter id
    <$> confirmDialog
          "Delete All Cards"
          (pure "Are you sure you want to delete all the cards?")
          deleteAllClickEv

initColumns :: M.Map Int Model
initColumns = M.fromList
  [ (0, Model "What went well?" M.empty)
  , (1, Model "What can we do better?" M.empty)
  , (2, Model "Questions and comments" M.empty)
  ]
