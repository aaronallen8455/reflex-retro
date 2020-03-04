{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Widget.Comments
  ( CommentState
  , CommentEvent
  , commentsWidget
  , applyCommentEvent
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
import           Widget.SimpleButton (simpleButton)

data CommentState =
  CommentState
    { _csContent :: T.Text
    } deriving (Show, Eq)

makeLenses ''CommentState
Aeson.deriveJSON Aeson.defaultOptions ''CommentState

instance ToMarkdown CommentState where
  toMarkdown = _csContent

data CommentEvent
  = AddComment T.Text
  | DeleteComment Int
  | EditComment Int T.Text

Aeson.deriveJSON Aeson.defaultOptions ''CommentEvent

applyCommentEvent :: CommentEvent -> M.Map Int CommentState -> M.Map Int CommentState
applyCommentEvent (AddComment txt) comMap
  | T.null txt = comMap
  | otherwise =
      let nxtId = maybe 0 (succ . fst) $ M.lookupMax comMap
       in M.insert nxtId (CommentState txt) comMap
applyCommentEvent (DeleteComment i) comMap =
  M.delete i comMap
applyCommentEvent (EditComment i txt) comMap
  | T.null txt = comMap
  | otherwise = M.adjust (csContent .~ txt) i comMap

isKeyEvent :: CommentEvent -> Bool
isKeyEvent (AddComment _) = True
isKeyEvent _ = False

commentsWidget :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
               => Dynamic t (M.Map Int CommentState) -> m (Event t CommentEvent)
commentsWidget comMapDyn = divClass "comments" $ do
  addCommentInputDyn <- _inputElement_value <$> inputElement def
  addCommentClickEv  <- simpleButton "Add Comment"

  let addCardEv = AddComment <$> current addCommentInputDyn
                             <@  addCommentClickEv

  commentWidgetEvents
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey comMapDyn commentWidget

  pure $ leftmost [addCardEv, commentWidgetEvents]

commentWidget :: (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m)
              => Int -> Dynamic t CommentState -> m (Event t CommentEvent)
commentWidget comId comStateDyn = divClass "comment" $ do
  editComEv <- elClass "div" "comment-content"
             . (fmap . fmap) (EditComment comId)
             $ editableText (_csContent <$> comStateDyn)

  deleteComEv <- (DeleteComment comId <$) <$> simpleButton "X"

  pure $ leftmost [editComEv, deleteComEv]
