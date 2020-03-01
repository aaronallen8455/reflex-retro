{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Widget.Columns
  ( ColumnState
  , ColumnEvent
  , columnsWidget
  , applyColumnEvent
  , initColumns
  ) where

import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import qualified Data.Aeson.TH as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import           Safe (headMay)

import           Reflex.Dom.Core

import qualified Widget.Cards as Cards
import           Widget.EditableText (editableText)

data ColumnState =
  ColumnState
    { _colTitle :: T.Text
    , _colCards :: M.Map Int Cards.CardState
    }

makeLenses ''ColumnState
Aeson.deriveJSON Aeson.defaultOptions ''ColumnState

data ColumnEvent
  = DeleteColumn Int
  | AddColumn T.Text
  | ColCardEvent Int Cards.CardEvent
  | ChangeTitle Int T.Text

Aeson.deriveJSON Aeson.defaultOptions ''ColumnEvent

applyColumnEvent :: ColumnEvent -> M.Map Int ColumnState -> M.Map Int ColumnState
applyColumnEvent (AddColumn colName) colMap
  | T.null colName = colMap
  | otherwise =
    let nxtId = maybe 0 (succ . fst) $ M.lookupMax colMap
     in M.insert nxtId (ColumnState colName M.empty) colMap
applyColumnEvent (DeleteColumn colId) colMap
  = M.delete colId colMap
applyColumnEvent (ColCardEvent colId cardEvent) colMap
  = M.adjust (colCards %~ Cards.applyCardEvent cardEvent) colId colMap
applyColumnEvent (ChangeTitle colId newTitle) colMap
  = M.adjust (colTitle .~ newTitle) colId colMap

columnsWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
              => Dynamic t (M.Map Int ColumnState) -> m (Event t ColumnEvent)
columnsWidget colMapDyn = do
  addColumnNameDyn <- _inputElement_value <$> inputElement def
  addColumnClickEv <- button "Add Column"
  let addColumnEv = AddColumn <$> current addColumnNameDyn
                              <@  addColumnClickEv

  columnWidgetEvents
    <- elClass "div" "columns"
     $ flip fforMaybe (headMay . M.elems)
   <$> listViewWithKey colMapDyn columnWidget

  pure $ leftmost [addColumnEv, columnWidgetEvents]

columnWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
             => Int -> Dynamic t ColumnState -> m (Event t ColumnEvent)
columnWidget colId colStateDyn = elClass "div" "column" $ do
  changeTitleEv
    <- (fmap . fmap) (ChangeTitle colId)
     . el "h3" $ editableText (_colTitle <$> colStateDyn)

  deleteColumnEv <- (DeleteColumn colId <$) <$> button "X"

  cardWidgetEvents <- Cards.cardsWidget $ _colCards <$> colStateDyn

  let cardEvents = ColCardEvent colId <$> cardWidgetEvents

  pure $ leftmost [ cardEvents
                  , deleteColumnEv
                  , changeTitleEv
                  ]

initColumns :: M.Map Int ColumnState
initColumns = M.fromList
  [ (0, ColumnState "What went well?" M.empty)
  , (1, ColumnState "What can we do better?" M.empty)
  , (2, ColumnState "Questions and comments" M.empty)
  ]
