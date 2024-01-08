module Sgf (parseSgf) where

import Data.Map  (Map)
import Data.Text (Text)
import Data.Tree (Tree)
import Data.Void (Void)
import Text.Parsec

type SGFParser = Parsec Text Void

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = error "You need to implement this function."

