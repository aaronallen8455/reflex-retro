{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Widget.Main
  ( FrontendState
  , FrontendEvent
  , initFrontendState
  , isKeyEvent
  , mkReplaceEvent
  , applyFrontendEvents
  , applyFrontendEvent
  , frontendWidget
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T

import qualified Language.Javascript.JSaddle.Evaluate as JS
import qualified Language.Javascript.JSaddle.Types as JS
import           Reflex.Dom.Core

import           Common.Markdown (ToMarkdown(..))
import qualified Widget.ActionItems as ActionItems
import qualified Widget.Columns as Columns
import           Widget.EditableText (editableText)
import           Widget.SimpleButton (simpleButton)

data FrontendState =
  FrontendState
    { _fsColumns     :: M.Map Int Columns.ColumnState
    , _fsActionItems :: ActionItems.ActionItems
    , _fsTitle       :: T.Text
    }

makeLenses ''FrontendState
Aeson.deriveJSON Aeson.defaultOptions ''FrontendState

instance ToMarkdown FrontendState where
  toMarkdown fs =
    T.unlines
      $ "# " <> _fsTitle fs
      : toMarkdown (_fsActionItems fs)
      : map toMarkdown (M.elems $ _fsColumns fs)

initFrontendState :: FrontendState
initFrontendState = FrontendState Columns.initColumns mempty "Retro"

data FrontendEvent
  = ColEvent Columns.ColumnEvent
  | ActionItemEvent ActionItems.ActionItemEvent
  | Replace FrontendState
  | ChangeTitle T.Text

Aeson.deriveJSON Aeson.defaultOptions ''FrontendEvent

isKeyEvent :: FrontendEvent -> Bool
isKeyEvent (ColEvent ev) = Columns.isKeyEvent ev
isKeyEvent (ActionItemEvent ev) = ActionItems.isKeyEvent ev
isKeyEvent _ = False

mkReplaceEvent :: FrontendState -> FrontendEvent
mkReplaceEvent = Replace

applyFrontendEvents :: NE.NonEmpty FrontendEvent -> FrontendState -> FrontendState
applyFrontendEvents = flip (foldr applyFrontendEvent)

applyFrontendEvent :: FrontendEvent -> FrontendState -> FrontendState
applyFrontendEvent (Replace fs) _ = fs
applyFrontendEvent (ColEvent ev) fs =
  fs & fsColumns %~ Columns.applyColumnEvent ev
applyFrontendEvent (ActionItemEvent ev) fs =
  fs & fsActionItems %~ ActionItems.applyActionItemEvent ev
applyFrontendEvent (ChangeTitle txt) fs
  | not $ T.null txt = fs & fsTitle .~ txt
  | otherwise = fs

frontendWidget :: ( DomBuilder t m, PostBuild t m, PerformEvent t m, JS.MonadJSM (Performable m), MonadHold t m, MonadFix m)
               => Dynamic t FrontendState -> m (Event t FrontendEvent)
frontendWidget stateDyn = do
  editTitleEv
    <- fmap ChangeTitle
   <$> elClass "h2" "title" (editableText (_fsTitle <$> stateDyn))

  columnEvents <- fmap ColEvent
              <$> Columns.columnsWidget (_fsColumns <$> stateDyn)

  actionItemEvents <- fmap ActionItemEvent
                  <$> ActionItems.actionItemsWidget (_fsActionItems <$> stateDyn)

  markdownToClipboardWidget stateDyn

  pure $ leftmost [ editTitleEv
                  , columnEvents
                  , actionItemEvents
                  ]

markdownToClipboardWidget :: (DomBuilder t m, PerformEvent t m, JS.MonadJSM (Performable m))
                          => Dynamic t FrontendState -> m ()
markdownToClipboardWidget stateDyn = do
  clipboardClickEv <- simpleButton "Copy Markdown to Clipboard"

  let markdownEv = toMarkdown
               <$> current stateDyn
               <@  clipboardClickEv

  performEvent_ . ffor markdownEv $ \md ->
    void . JS.liftJSM . JS.eval
      $ "navigator.clipboard.writeText(`" <> md <> "`)"

