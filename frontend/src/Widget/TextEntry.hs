{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Widget.TextEntry
  ( textEntry
  ) where

import           Control.Monad.Fix (MonadFix)
import qualified Data.Text as T

import           Reflex.Dom.Core

textEntry :: (DomBuilder t m, MonadFix m)
          => T.Text
          -> m (Event t (Either Key T.Text))
textEntry placeHolder = mdo
  let attrs = ("placeholder" =: placeHolder)
           <> ("class" =: "text-input")

      inpElConfig =
        def & inputElementConfig_setValue .~ clearInpEv
            & inputElementConfig_elementConfig . elementConfig_initialAttributes
                .~ attrs

  inp <- inputElement inpElConfig

  let inpEl = _inputElement_element inp
      keydownEvent = keyCodeLookup . fromIntegral
                 <$> domEvent Keydown inpEl
      enterKeyEv = ffilter (== Enter) keydownEvent
      clearInpEv = "" <$ enterKeyEv

      resultEv =
        leftmost
          [ Right <$> tag (current $ _inputElement_value inp) enterKeyEv
          , Left <$> keydownEvent
          ]

  pure resultEv

