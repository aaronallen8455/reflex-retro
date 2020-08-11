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
  , isActivityEvent
  , mkReplaceEvent
  , applyEvents
  , applyEvent
  , widget
  , mkInactivityEvent
  ) where

import           Control.Lens
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import           Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time (diffUTCTime, getCurrentTime)

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
    { _fsColumns        :: M.Map Int Columns.Model
    , _fsActionItems    :: ActionItems.Model
    , _fsTitle          :: T.Text
    , _fsTypingActivity :: TypingActivityStatus
    }

data TypingActivityStatus
  = SomeoneTyping
  | NobodyTyping

makeLenses ''Model
Aeson.deriveJSON Aeson.defaultOptions ''TypingActivityStatus
Aeson.deriveJSON Aeson.defaultOptions ''Model

instance ToMarkdown Model where
  toMarkdown fs =
    T.unlines
      $ "# " <> _fsTitle fs
      : map toMarkdown (M.elems $ _fsColumns fs)
     <> [toMarkdown (_fsActionItems fs)]

initModel :: Model
initModel =
  Model
    { _fsColumns        = Columns.initColumns
    , _fsActionItems    = mempty
    , _fsTitle          = "Retro"
    , _fsTypingActivity = NobodyTyping
    }

data Ev
  = ColEvent Columns.Ev
  | ActionItemEvent ActionItems.Ev
  | Replace Model
  | ChangeTitle T.Text
  | Activity
  | Inactivity

Aeson.deriveJSON Aeson.defaultOptions ''Ev

-- The server needs to be able to emit this event
mkInactivityEvent :: Ev
mkInactivityEvent = Inactivity

-- Key events are events for which the server should emit the entire model to
-- synchronize the emitter of the event with the server's model.
isKeyEvent :: Ev -> Bool
isKeyEvent (ColEvent ev) = Columns.isKeyEvent ev
isKeyEvent (ActionItemEvent ev) = ActionItems.isKeyEvent ev
isKeyEvent _ = False

-- Activity events signal that someone is typing
isActivityEvent :: Ev -> Bool
isActivityEvent Activity = True
isActivityEvent (ColEvent ev) = Columns.isActivityEvent ev
isActivityEvent (ActionItemEvent ev) = ActionItems.isActivityEvent ev
isActivityEvent _ = False

mkReplaceEvent :: Model -> Ev
mkReplaceEvent = Replace

applyEvents :: NE.NonEmpty Ev -> Model -> Model
applyEvents = flip (foldr applyEvent)

applyEvent :: Ev -> Model -> Model
applyEvent ev fs
  | isActivityEvent ev = fs & fsTypingActivity .~ SomeoneTyping
applyEvent (Replace fs) _ = fs
applyEvent (ColEvent ev) fs =
  fs & fsColumns %~ Columns.applyEvent ev
applyEvent (ActionItemEvent ev) fs =
  fs & fsActionItems %~ ActionItems.applyEvent ev
applyEvent (ChangeTitle txt) fs
  | not $ T.null txt =
      fs & fsTitle .~ txt
  | otherwise = fs
applyEvent Activity fs = fs
applyEvent Inactivity fs =
  fs & fsTypingActivity .~ NobodyTyping

widget :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m, PerformEvent t m, JS.MonadJSM (Performable m), MonadHold t m, MonadFix m)
       => Dynamic t Model -> m (Event t Ev)
widget modelDyn = do
  editTitleEv
    <- fmap (either (const Activity) ChangeTitle)
   <$> elClass "h2" "title" (editableText (_fsTitle <$> modelDyn))

  markdownToClipboardWidget modelDyn

  let activityMonitor =
        activityMonitorWidget (_fsTypingActivity <$> modelDyn)

  columnEvents <- fmap ColEvent
              <$> Columns.widget (_fsColumns <$> modelDyn) activityMonitor

  actionItemEvents <- fmap ActionItemEvent
                  <$> ActionItems.widget (_fsActionItems <$> modelDyn)

  let event =
        leftmost [ editTitleEv
                 , columnEvents
                 , actionItemEvents
                 ]

  -- Activity events get batched to avoid extraneous network noise
  let zipWithTime e = -- attach the current time if it's an activity event
        if isActivityEvent e
           then (,) e . Just <$> liftIO getCurrentTime
           else pure (e, Nothing)

      compareTimes (e, t) (_, Nothing) = Just (Just e, t)
      compareTimes (e, Just new) (_, Just cur)
        -- don't send more than 1 activity event per 3 second period
        | diffUTCTime new cur > 3
        = Just (Just e, Just new)
        | otherwise = Nothing -- suppresses the event
      compareTimes (e, _) (_, t) = Just (Just e, t)

  eventAndTime <- performEvent $ zipWithTime <$> event

  eventAndLastActivityTime
    <- foldDynMaybe
         compareTimes
         (Nothing, Nothing)
         eventAndTime

  pure . fmapMaybe fst $ updated eventAndLastActivityTime

markdownToClipboardWidget :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PerformEvent t m, JS.MonadJSM (Performable m))
                          => Dynamic t Model -> m ()
markdownToClipboardWidget modelDyn = do
  clipboardClickEv <- buttonClass "markdown-button fancy-button"
                                  "Copy Markdown to Clipboard"

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

activityMonitorWidget :: (DomBuilder t m, PostBuild t m)
                      => Dynamic t TypingActivityStatus -> m ()
activityMonitorWidget activityDyn =
  divClass "activity-level-monitor" $ do
    text "Activity monitor: "
    dynText $ activityLevelToText <$> activityDyn
  where
    activityLevelToText SomeoneTyping = "Someone is typing"
    activityLevelToText NobodyTyping  = "Nobody typing"

