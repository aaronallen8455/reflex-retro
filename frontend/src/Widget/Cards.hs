{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Widget.Cards
  ( CardState
  , CardEvent
  , cardsWidget
  , applyCardEvent
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
import           Widget.SimpleButton (simpleButton)

data CardState =
  CardState
    { _cardText     :: T.Text
    , _cardLikes    :: Int
    , _cardComments :: M.Map Int Comments.CommentState
    } deriving (Show, Eq)

makeLenses ''CardState
Aeson.deriveJSON Aeson.defaultOptions ''CardState

instance ToMarkdown CardState where
  toMarkdown cs =
    T.unlines
      $ "(" <> likes <> ") " <> _cardText cs
      : map (("  - " <>) . toMarkdown)
            (M.elems $ _cardComments cs)
    where
      likes | _cardLikes cs > 0 = "+" <> likeTxt
            | otherwise = likeTxt
            where
              likeTxt = T.pack . show $ _cardLikes cs

data CardEvent
  = DeleteCard Int
  | AddCard T.Text
  | UpVote Int
  | DownVote Int
  | ContentChange Int T.Text
  | CardCommentEvent Int Comments.CommentEvent

Aeson.deriveJSON Aeson.defaultOptions ''CardEvent

applyCardEvent :: CardEvent -> M.Map Int CardState -> M.Map Int CardState
applyCardEvent (DeleteCard i) cardMap = M.delete i cardMap
applyCardEvent (AddCard txt) cardMap
  | T.null txt = cardMap
  | otherwise =
      let nxt = maybe 0 (succ . fst) $ M.lookupMax cardMap
       in M.insert nxt (CardState txt 0 M.empty) cardMap
applyCardEvent (UpVote i) cardMap =
  M.adjust (cardLikes +~ 1) i cardMap
applyCardEvent (DownVote i) cardMap =
  M.adjust (cardLikes -~ 1) i cardMap
applyCardEvent (ContentChange i newTxt) cardMap
  | T.null newTxt = cardMap
  | otherwise = M.adjust (cardText .~ newTxt) i cardMap
applyCardEvent (CardCommentEvent i ev) cardMap =
  M.adjust (cardComments %~ Comments.applyCommentEvent ev) i cardMap

cardsWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
            => Dynamic t (M.Map Int CardState) -> m (Event t CardEvent)
cardsWidget cardMapDyn = do
  addCardInputDyn <- _inputElement_value <$> inputElement def
  addCardClickEv  <- simpleButton "Add Card"

  let addCardEv = AddCard <$> current addCardInputDyn
                          <@  addCardClickEv

  cardWidgetEvents
    <- fmapMaybe (headMay . M.elems)
   <$> listViewWithKey cardMapDyn cardWidget

  pure $ leftmost [addCardEv, cardWidgetEvents]

cardWidget :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m)
           => Int -> Dynamic t CardState -> m (Event t CardEvent)
cardWidget cardId cardStateDyn = elClass "div" "card" $ do
  contentChangeEv
    <- (fmap . fmap) (ContentChange cardId)
     . elClass "div" "card-content" . editableText $ _cardText <$> cardStateDyn
  elClass "span" "up-votes" . dynText
    $ T.pack . show . _cardLikes <$> cardStateDyn

  deleteCardEv <- (DeleteCard cardId <$) <$> simpleButton "x"
  upVoteCardEv <- (UpVote cardId <$) <$> simpleButton "+1"
  downVoteCardEv <- (DownVote cardId <$) <$> simpleButton "-1"

  commentEvents <- fmap (CardCommentEvent cardId)
               <$> Comments.commentsWidget (_cardComments <$> cardStateDyn)

  pure $ leftmost [ contentChangeEv
                  , deleteCardEv
                  , upVoteCardEv
                  , downVoteCardEv
                  , commentEvents
                  ]

