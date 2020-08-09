{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
module Widget.Cards
  ( Model
  , Ev
  , widget
  , applyEvent
  , isKeyEvent
  , isActivityEvent
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
import qualified Widget.Comments as Comments
import           Widget.EditableText (editableText)
import           Widget.SimpleButton (buttonClass, simpleButton)
import           Widget.TextEntry (textEntry)

data Model =
  Model
    { _cardText     :: T.Text
    , _cardLikes    :: Int
    , _cardComments :: M.Map Int Comments.Model
    } deriving (Show, Eq)

makeLenses ''Model
Aeson.deriveJSON Aeson.defaultOptions ''Model

instance ToMarkdown Model where
  toMarkdown cs =
    _cardText cs <> " " <> likes
    <> foldMap (("\n  - " <>) . toMarkdown)
               (M.elems $ _cardComments cs)
    where
      likes | _cardLikes cs == 0 = ""
            | _cardLikes cs > 0 = "(+" <> likeTxt <> ")"
            | otherwise = "(" <> likeTxt <> ")"
            where
              likeTxt = T.pack . show $ _cardLikes cs

data Ev
  = DeleteCard Int
  | AddCard T.Text
  | UpVote Int
  | DownVote Int
  | ContentChange Int T.Text
  | CardCommentEvent Int Comments.Ev
  | Activity

Aeson.deriveJSON Aeson.defaultOptions ''Ev

applyEvent :: Ev -> M.Map Int Model -> M.Map Int Model
applyEvent (DeleteCard i) cardMap = M.delete i cardMap
applyEvent (AddCard txt) cardMap
  | T.null txt = cardMap
  | otherwise =
      let nxt = maybe 0 (succ . fst) $ M.lookupMax cardMap
       in M.insert nxt (Model txt 0 M.empty) cardMap
applyEvent (UpVote i) cardMap =
  M.adjust (cardLikes +~ 1) i cardMap
applyEvent (DownVote i) cardMap =
  M.adjust (cardLikes -~ 1) i cardMap
applyEvent (ContentChange i newTxt) cardMap
  | T.null newTxt = cardMap
  | otherwise = M.adjust (cardText .~ newTxt) i cardMap
applyEvent (CardCommentEvent i ev) cardMap =
  M.adjust (cardComments %~ Comments.applyEvent ev) i cardMap
applyEvent Activity cardMap = cardMap

isKeyEvent :: Ev -> Bool
isKeyEvent (AddCard _) = True
isKeyEvent (CardCommentEvent _ ev) = Comments.isKeyEvent ev
isKeyEvent _ = False

isActivityEvent :: Ev -> Bool
isActivityEvent Activity = True
isActivityEvent (CardCommentEvent _ e) = Comments.isActivityEvent e
isActivityEvent _ = False

widget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
       => Dynamic t (M.Map Int Model) -> m (Event t Ev)
widget cardMapDyn = mdo
  addCardEv <- fmap (either (const Activity) AddCard)
           <$> textEntry "Add Card..."

  cardWidgetEvents
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey cardMapDyn cardWidget

  pure $ leftmost [addCardEv, cardWidgetEvents]

cardWidget :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m)
           => Int -> Dynamic t Model -> m (Event t Ev)
cardWidget cardId modelDyn = elClass "div" "card" $ do
  contentChangeEv
    <- (fmap . fmap) (either (const Activity) (ContentChange cardId))
     . elClass "div" "card-content" . editableText $ _cardText <$> modelDyn
  elClass "span" "up-votes" . dynText
    $ T.pack . show . _cardLikes <$> modelDyn

  deleteCardEv <- (DeleteCard cardId <$) <$> buttonClass "delete-button" "×"
  upVoteCardEv <- (UpVote cardId <$) <$> simpleButton "+1"
  downVoteCardEv <- (DownVote cardId <$) <$> simpleButton "-1"

  commentEvents <- fmap (CardCommentEvent cardId)
               <$> Comments.widget (_cardComments <$> modelDyn)

  pure $ leftmost [ contentChangeEv
                  , deleteCardEv
                  , upVoteCardEv
                  , downVoteCardEv
                  , commentEvents
                  ]

