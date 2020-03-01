module Common.Markdown
  ( ToMarkdown(..)
  ) where

import qualified Data.Text as T

class ToMarkdown a where
  toMarkdown :: a -> T.Text

