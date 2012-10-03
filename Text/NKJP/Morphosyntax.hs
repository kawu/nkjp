{-# LANGUAGE OverloadedStrings #-}

module Text.NKJP.Morphosyntax
( parseMorph
, readMorph
, readCorpus
, module Data.NKJP.Morphosyntax
) where

import System.FilePath (takeBaseName)
import Data.Maybe (isJust)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Text.XML.PolySoup
import Data.NKJP.Morphosyntax

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

parseMorph :: L.Text -> [Para L.Text]
parseMorph = parseXml morphP

-- | Parse the stand-alone ann_morphosyntax.xml file.
readMorph :: FilePath -> IO [Para L.Text]
readMorph morphPath = parseMorph <$> L.readFile morphPath

-- | Parse NCP the .tar.gz corpus.
readCorpus :: FilePath -> IO [(FilePath, [Para L.Text])]
readCorpus tarPath = do
    map parseEntry . withBase "ann_morphosyntax" <$> readTar tarPath

readTar :: FilePath -> IO [Tar.Entry]
readTar tar
    =  Tar.foldEntries (:) [] error
    .  Tar.read . GZip.decompress
   <$> BS.readFile tar

parseEntry :: Tar.Entry -> (FilePath, [Para L.Text])
parseEntry entry =
    (Tar.entryPath entry, parseMorph content)
  where
    (Tar.NormalFile binary _) = Tar.entryContent entry
    content = L.decodeUtf8 binary

withBase :: String -> [Tar.Entry] -> [Tar.Entry]
withBase baseName = filter ((==baseName) . takeBaseName . Tar.entryPath)
