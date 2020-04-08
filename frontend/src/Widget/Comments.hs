{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Widget.Comments
  ( Model
  , Ev
  , widget
  , applyEvent
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
import           Widget.EditableText (editableText)
import           Widget.SimpleButton (buttonClass)
import           Widget.TextEntry (textEntry)

data Model =
  Model
    { _csContent :: T.Text
    } deriving (Show, Eq)

makeLenses ''Model
Aeson.deriveJSON Aeson.defaultOptions ''Model

instance ToMarkdown Model where
  toMarkdown = _csContent

data Ev
  = AddComment T.Text
  | DeleteComment Int
  | EditComment Int T.Text

Aeson.deriveJSON Aeson.defaultOptions ''Ev

applyEvent :: Ev -> M.Map Int Model -> M.Map Int Model
applyEvent (AddComment txt) comMap
  | T.null txt = comMap
  | otherwise =
      let nxtId = maybe 0 (succ . fst) $ M.lookupMax comMap
       in M.insert nxtId (Model txt) comMap
applyEvent (DeleteComment i) comMap =
  M.delete i comMap
applyEvent (EditComment i txt) comMap
  | T.null txt = comMap
  | otherwise = M.adjust (csContent .~ txt) i comMap

isKeyEvent :: Ev -> Bool
isKeyEvent (AddComment _) = True
isKeyEvent _ = False

widget :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
       => Dynamic t (M.Map Int Model) -> m (Event t Ev)
widget comMapDyn = divClass "comments" $ mdo
  addCommentEv <- fmap AddComment
              <$> textEntry "Add Comment..."

  commentWidgetEvents
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey comMapDyn commentWidget

  pure $ leftmost [addCommentEv, commentWidgetEvents]

commentWidget :: (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m)
              => Int -> Dynamic t Model -> m (Event t Ev)
commentWidget comId modelDyn = divClass "comment" $ do
  editComEv <- elClass "div" "comment-content"
             . (fmap . fmap) (EditComment comId)
             $ editableText (_csContent <$> modelDyn)

  deleteComEv <- (DeleteComment comId <$) <$> buttonClass "delete-button" "×"

  pure $ leftmost [editComEv, deleteComEv]
