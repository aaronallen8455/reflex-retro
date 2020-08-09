{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Widget.Comments
  ( Model
  , Ev
  , widget
  , applyEvent
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
import           Widget.EditableText (editableText)
import           Widget.SimpleButton (buttonClass)
import           Widget.TextEntry (textEntry)

newtype Model =
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
  | Activity

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
applyEvent Activity comMap = comMap

isKeyEvent :: Ev -> Bool
isKeyEvent (AddComment _) = True
isKeyEvent _ = False

isActivityEvent :: Ev -> Bool
isActivityEvent Activity = True
isActivityEvent _ = False

widget :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
       => Dynamic t (M.Map Int Model) -> m (Event t Ev)
widget comMapDyn = divClass "comments" $ mdo
  addCommentEv <- fmap (either (const Activity) AddComment)
              <$> textEntry "Add Comment..."

  commentWidgetEvents
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey comMapDyn commentWidget

  pure $ leftmost [addCommentEv, commentWidgetEvents]

commentWidget :: (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m)
              => Int -> Dynamic t Model -> m (Event t Ev)
commentWidget comId modelDyn = divClass "comment" $ do
  editComEv <- elClass "div" "comment-content"
             . (fmap . fmap) (either (const Activity) (EditComment comId))
             $ editableText (_csContent <$> modelDyn)

  deleteComEv <- (DeleteComment comId <$) <$> buttonClass "delete-button" "×"

  pure $ leftmost [editComEv, deleteComEv]
