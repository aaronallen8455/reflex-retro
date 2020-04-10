{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
module Widget.Main
  ( Model
  , Ev
  , initModel
  , isKeyEvent
  , mkReplaceEvent
  , applyEvents
  , applyEvent
  , widget
  ) where

import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import           Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T

import qualified GHCJS.DOM.HTMLTextAreaElement as DOM
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM as DOM
import qualified Language.Javascript.JSaddle.Types as JS
import           Reflex.Dom.Core

import           Common.Markdown (ToMarkdown(..))
import qualified Widget.ActionItems as ActionItems
import qualified Widget.Columns as Columns
import           Widget.EditableText (editableText)
import           Widget.SimpleButton (buttonClass)

data Model =
  Model
    { _fsColumns     :: M.Map Int Columns.Model
    , _fsActionItems :: ActionItems.Model
    , _fsTitle       :: T.Text
    }

makeLenses ''Model
Aeson.deriveJSON Aeson.defaultOptions ''Model

instance ToMarkdown Model where
  toMarkdown fs =
    T.unlines
      $ "# " <> _fsTitle fs
      : map toMarkdown (M.elems $ _fsColumns fs)
     <> [toMarkdown (_fsActionItems fs)]

initModel :: Model
initModel = Model Columns.initColumns mempty "Retro"

data Ev
  = ColEvent Columns.Ev
  | ActionItemEvent ActionItems.Ev
  | Replace Model
  | ChangeTitle T.Text

Aeson.deriveJSON Aeson.defaultOptions ''Ev

isKeyEvent :: Ev -> Bool
isKeyEvent (ColEvent ev) = Columns.isKeyEvent ev
isKeyEvent (ActionItemEvent ev) = ActionItems.isKeyEvent ev
isKeyEvent _ = False

mkReplaceEvent :: Model -> Ev
mkReplaceEvent = Replace

applyEvents :: NE.NonEmpty Ev -> Model -> Model
applyEvents = flip (foldr applyEvent)

applyEvent :: Ev -> Model -> Model
applyEvent (Replace fs) _ = fs
applyEvent (ColEvent ev) fs =
  fs & fsColumns %~ Columns.applyEvent ev
applyEvent (ActionItemEvent ev) fs =
  fs & fsActionItems %~ ActionItems.applyEvent ev
applyEvent (ChangeTitle txt) fs
  | not $ T.null txt = fs & fsTitle .~ txt
  | otherwise = fs

widget :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m, PerformEvent t m, JS.MonadJSM (Performable m), MonadHold t m, MonadFix m)
       => Dynamic t Model -> m (Event t Ev)
widget modelDyn = do
  editTitleEv
    <- fmap ChangeTitle
   <$> elClass "h2" "title" (editableText (_fsTitle <$> modelDyn))

  markdownToClipboardWidget modelDyn

  columnEvents <- fmap ColEvent
              <$> Columns.widget (_fsColumns <$> modelDyn)

  actionItemEvents <- fmap ActionItemEvent
                  <$> ActionItems.widget (_fsActionItems <$> modelDyn)

  pure $ leftmost [ editTitleEv
                  , columnEvents
                  , actionItemEvents
                  ]

markdownToClipboardWidget :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PerformEvent t m, JS.MonadJSM (Performable m))
                          => Dynamic t Model -> m ()
markdownToClipboardWidget modelDyn = do
  clipboardClickEv <- buttonClass "markdown-button" "Copy Markdown to Clipboard"

  let markdownEv = toMarkdown
               <$> current modelDyn
               <@  clipboardClickEv

  mdTextArea <- divClass "markdown-output" . textAreaElement $
    def & textAreaElementConfig_setValue .~ markdownEv

  let copyToClipBoard = do
        DOM.select $ _textAreaElement_raw mdTextArea
        mbDoc <- DOM.currentDocument
        for_ mbDoc $ \doc ->
          DOM.execCommand_ doc ("copy" :: String) False (Nothing :: Maybe String)

  performEvent_
    $ copyToClipBoard <$ updated (_textAreaElement_value mdTextArea)

