{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Parsing the NKJP morphosyntax layer.

module Text.NKJP.Morphosyntax
(
-- * Data types
  Para (..)
, Sent (..)
, Seg (..)
, Lex (..)

-- * Parsing
, parseMorph
, readMorph
, readCorpus
) where

import Data.Maybe (isJust)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Text.XML.PolySoup
import qualified Text.NKJP.Tar as Tar

-- | A paragraph.
data Para t = Para
    { paraID    :: t
    , sentences :: [Sent t] }
    deriving (Show, Functor)

-- | A sentence.
data Sent t = Sent
    { sentID    :: t
    , segments  :: [Seg t] }
    deriving (Show, Functor)

-- | A segment.
data Seg t = Seg
    { segID     :: t 
    , orth      :: t
    , nps       :: Bool
    , lexs      :: [Lex t]
    , choice    :: (t, t) }
    deriving (Show, Functor)

-- | A lexciacal entry, potential interpretation of the segment.
data Lex t = Lex
    { lexID     :: t
    , base      :: t
    , ctag      :: t
    , msds      :: [(t, t)] }
    deriving (Show, Functor)

-- | TEI NKJP ann_morphosyntax parser.
type P a = XmlParser L.Text a

morphP :: P [Para L.Text]
morphP = true //> paraP

paraP :: P (Para L.Text)
paraP = uncurry Para <$> (tag "p" *> getAttr "xml:id" </> sentP)

sentP :: P (Sent L.Text)
sentP = uncurry Sent <$> (tag "s" *> getAttr "xml:id" </> segP)

segP :: P (Seg L.Text)
segP = (tag "seg" *> getAttr "xml:id") `join` smP

smP :: L.Text -> P (Seg L.Text)
smP _segID = (tag "fs" *> hasAttr "type" "morph") `joinR` ( Seg
    <$> pure _segID
    <*> fStrP "orth"
    <*> (isJust <$> (optional . cut $ hasAttr "name" "nps"))
    <*> (hasAttr "name" "interps" /> lexP)
    <*> choiceP )

lexP :: P (Lex L.Text)
lexP = (hasAttr "type" "lex" *> getAttr "xml:id") `join` \_lexID -> ( Lex
    <$> pure _lexID
    <*> fStrP "base"
    <*> fSymP "ctag"
    <*> (hasAttr "name" "msd" //> cut
        ((,) <$> (tag "symbol" *> getAttr "xml:id") <*> getAttr "value")) )

choiceP :: P (L.Text, L.Text)
choiceP = hasAttr "name" "disamb" `joinR` ( tag "fs" `joinR` do
    ptr <- L.tail <$> cut (getAttr "fVal")
    interp <- fStrP "interpretation"
    return (ptr, interp) )

fStrP :: L.Text -> P L.Text
fStrP x =
    let checkName = tag "f" *> hasAttr "name" x
        -- | Body sometimes is empty.
        safeHead [] = ""
        safeHead xs = head xs
    in  safeHead <$> (checkName #> tag "string" /> text)

fSymP :: L.Text -> P L.Text
fSymP x =
    let checkName = tag "f" *> hasAttr "name" x
        p = cut (tag "symbol" *> getAttr "value")
    in  head <$> (checkName /> p)

-- | Parse textual contents of the ann_morphosyntax.xml file.
parseMorph :: L.Text -> [Para L.Text]
parseMorph = parseXml morphP

-- | Parse the stand-alone ann_morphosyntax.xml file.
readMorph :: FilePath -> IO [Para L.Text]
readMorph morphPath = parseMorph <$> L.readFile morphPath

-- | Parse all ann_morphosyntax.xml files from the NCP .tar.gz file.
readCorpus :: FilePath -> IO [(FilePath, Maybe [Para L.Text])]
readCorpus = Tar.readCorpus "ann_morphosyntax" parseMorph
