{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Widget.Columns
  ( Model
  , Ev
  , widget
  , applyEvent
  , initColumns
  , isKeyEvent
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
import           Widget.EditableText (editableText)
import           Widget.SimpleButton (buttonClass)
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
      : map (\(i, c) -> T.pack (show i) <> ". " <> toMarkdown c)
            ([1 :: Int ..] `zip` M.elems (_colCards cs))

data Ev
  = DeleteColumn Int
  | AddColumn T.Text
  | ColCardEvent Int Cards.Ev
  | ChangeTitle Int T.Text

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

isKeyEvent :: Ev -> Bool
isKeyEvent (AddColumn _) = True
isKeyEvent (ColCardEvent _ ev) = Cards.isKeyEvent ev
isKeyEvent _ = False

widget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
       => Dynamic t (M.Map Int Model) -> m (Event t Ev)
widget colMapDyn = do
  addColumnEv <- fmap AddColumn
             <$> textEntry "Add Column..."

  columnWidgetEvents
    <- elClass "div" "columns"
     $ flip fforMaybe (headMay . M.elems)
   <$> listViewWithKey colMapDyn columnWidget

  pure $ leftmost [addColumnEv, columnWidgetEvents]

columnWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
             => Int -> Dynamic t Model -> m (Event t Ev)
columnWidget colId modelDyn = elClass "div" "column" $ do
  changeTitleEv
    <- (fmap . fmap) (ChangeTitle colId)
     . el "h3" $ editableText (_colTitle <$> modelDyn)

  deleteColumnEv <- (DeleteColumn colId <$) <$> buttonClass "delete-button" "×"

  cardWidgetEvents <- Cards.widget $ _colCards <$> modelDyn

  let cardEvents = ColCardEvent colId <$> cardWidgetEvents

  pure $ leftmost [ cardEvents
                  , deleteColumnEv
                  , changeTitleEv
                  ]

initColumns :: M.Map Int Model
initColumns = M.fromList
  [ (0, Model "What went well?" M.empty)
  , (1, Model "What can we do better?" M.empty)
  , (2, Model "Questions and comments" M.empty)
  ]
