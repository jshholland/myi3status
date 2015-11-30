{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative (empty)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as C
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding
import System.Process
import System.IO

-- Parser for i3bar protocol

type Color = Text

newtype MinWidth = MinWidth Int
  deriving (Eq, Show)

instance FromJSON MinWidth where
  parseJSON (String v) = pure . MinWidth . T.length $ v
  parseJSON (Number v) = pure . MinWidth . ceiling $ v
  parseJSON _ = empty

instance ToJSON MinWidth where
  toJSON (MinWidth w) = toJSON w

data Alignment = LeftAlign | CenterAlign | RightAlign
  deriving (Show, Eq)

instance FromJSON Alignment where
  parseJSON (String v) = case v of
                           "left" -> pure LeftAlign
                           "center" -> pure CenterAlign
                           "right" -> pure RightAlign
                           _ -> empty
  parseJSON _ = empty

instance ToJSON Alignment where
  toJSON LeftAlign   = "left"
  toJSON CenterAlign = "center"
  toJSON RightAlign  = "right"

data Markup = Pango | NoMarkup
  deriving (Show, Eq)

instance FromJSON Markup where
  parseJSON (String v) = case v of
                           "pango" -> pure Pango
                           "none" -> pure NoMarkup
                           _ -> empty

instance ToJSON Markup where
  toJSON Pango    = "pango"
  toJSON NoMarkup = "none"

data Block = Block {
    _fullText            :: Text
  , _shortText           :: Maybe Text
  , _color               :: Maybe Color
  , _minWidth            :: Maybe MinWidth
  , _align               :: Maybe Alignment
  , _name                :: Maybe Text
  , _inst                :: Maybe Text
  , _urgent              :: Maybe Bool
  , _separator           :: Maybe Bool
  , _separatorBlockWidth :: Maybe Int
  , _markup              :: Maybe Markup
  } deriving (Show, Eq)

makeLenses ''Block

instance FromJSON Block where
  parseJSON (Object v) = Block <$>
    v .: "full_text" <*>
    v .:? "short_text" <*>
    v .:? "color" <*>
    v .:? "min_width" <*>
    v .:? "align" <*>
    v .:? "name" <*>
    v .:? "instance" <*>
    v .:? "urgent" <*>
    v .:? "separator" <*>
    v .:? "separator_block_width" <*>
    v .:? "markup"

instance ToJSON Block where
  toJSON b = object $ "full_text" .= view fullText b : catMaybes
    [ ("short_text" .=)            <$> view shortText b
    , ("color" .=)                 <$> view color b
    , ("min_width" .=)             <$> view minWidth b
    , ("align" .=)                 <$> view align b
    , ("name" .=)                  <$> view name b
    , ("instance" .=)              <$> view inst b
    , ("urgent" .=)                <$> view urgent b
    , ("separator" .=)             <$> view separator b
    , ("separator_block_width" .=) <$> view separatorBlockWidth b
    , ("markup" .=)                <$> view markup b
    ]

block :: Text -> Block
block fullText = Block {
    _fullText            = fullText
  , _shortText           = Nothing
  , _color               = Nothing
  , _minWidth            = Nothing
  , _align               = Nothing
  , _name                = Nothing
  , _inst                = Nothing
  , _urgent              = Nothing
  , _separator           = Nothing
  , _separatorBlockWidth = Nothing
  , _markup              = Nothing
  }

-- Actually wrap the status output

processBlocks :: [Block] -> IO ByteString
processBlocks bs = appendNewline . LB.toStrict . encode . (: bs) <$> mail
  where appendNewline = flip B.snoc . fromIntegral . fromEnum $ '\n'

processLines :: Conduit Text IO ByteString
processLines = CL.mapM $ \line ->
  let (pref, bline) = over both encodeUtf8 $ T.span (== ',') line
  in fmap (pref <>) $ maybe (return bline) processBlocks $ decode . LB.fromStrict $ bline

-- Modules to get extra data

mail :: IO Block
mail = do
  unread <- init <$> readProcess "notmuch-remote"
                                 ["count", "tag:inbox", "tag:unread"]
                                 ""
  inbox <- init <$> readProcess "notmuch-remote"
                                ["count", "tag:inbox"]
                                ""
  let t = T.pack $ "E: " ++ unread ++ "/" ++ inbox
  return $ block t & name .~ Just "email"

main :: IO ()
main = C.stdin $= C.linesUnbounded =$= processLines $$ C.stdout
