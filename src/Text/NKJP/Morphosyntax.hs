{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
, chosen

-- * Parsing
, parseMorph
, readMorph
, readCorpus
) where

import           Control.Applicative
import qualified Data.Foldable as F
import           Data.Maybe (isJust)
import           Data.List (find)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Text.HTML.TagSoup as TagSoup
-- import qualified Text.XML.PolySoup as PolySoup
import           Text.XML.PolySoup hiding (P, Q)

-- import qualified Text.NKJP.Tar as Tar
import qualified Text.NKJP.Corpus as C
import           Text.NKJP.Ptr
import           Text.NKJP.Utils


-------------------------------------------------
-- Data types
-------------------------------------------------


-- | A paragraph.
data Para t = Para
    { paraID    :: t
    , paraPtr   :: Ptr t
    , sentences :: [Sent t] }
    deriving (Show, Functor)

-- | A sentence.
data Sent t = Sent
    { sentID    :: t
    , sentPtr   :: Ptr t
    , segments  :: [Seg t] }
    deriving (Show, Functor)

-- | A segment.
data Seg t = Seg
    { segID     ::  t 
    , segPtr    :: Ptr t
    , orth      :: t
    , nps       :: Bool
    , lexs      :: [Lex t]
    -- | The first element is a pointer.
    , choice    :: (t, t) }
    deriving (Show, Functor)

-- | A lexciacal entry, potential interpretation of the segment.
data Lex t = Lex
    { lexID     :: t
    , base      :: t
    , ctag      :: t
    -- | First elements are identifiers.
    , msds      :: [(t, t)] }
    deriving (Show, Functor)


-- | Determine the disambiguated base form, class tag and MSD tag.
chosen :: (Show t, Eq t) => Seg t -> (t, t, t)
chosen Seg{..} =
    let (Lex{..}, msd) = findLex (fst choice) lexs
    in  (base, ctag, msd)


-- | Find `Lex` with the given MSD pointer.
-- The second argument returned is the target MSD.
findLex :: (Show t, Eq t) => t -> [Lex t] -> (Lex t, t)
findLex ptr =
    unJust . foldl (<|>) Nothing . map ddRef
  where
    ddRef x = (x,) <$> deRef ptr x
    unJust (Just x) = x
    unJust Nothing  = error $ concat
        [ "NKJP.Morphosyntax.findLex: no lexical element with the ID = "
        , show ptr ]

-- | Dereference the given MSD pointer.
deRef :: Eq t => t -> Lex t -> Maybe t
deRef x = fmap snd . find ((==x) . fst) . msds


-------------------------------------------------
-- Parsing
-------------------------------------------------


morphP :: P [Para L.Text]
morphP = concat <$> every' morphQ

morphQ :: Q [Para L.Text]
morphQ = true //> paraQ

paraQ :: Q (Para L.Text)
paraQ =
    let mkPara ((crp, xid), xs) = Para
            { paraID    = xid
            , paraPtr   = crp
            , sentences = xs }
    in  mkPara <$> (named "p" *> idesQ </> sentQ)

sentQ :: Q (Sent L.Text)
sentQ =
    let mkSent ((crp, xid), xs) = Sent
            { sentID    = xid
            , sentPtr   = crp
            , segments  = xs }
    in  mkSent <$> (named "s" *> idesQ </> segQ)

segQ :: Q (Seg L.Text)
segQ =  (named "seg" *> idesQ) `join` (first . smQ)

smQ :: (Ptr L.Text, L.Text) -> Q (Seg L.Text)
smQ (_corresp, _segID) = (named "fs" *> hasAttrVal "type" "morph") `joinR` ( Seg
    <$> pure _segID
    <*> pure _corresp
    <*> first (fStrQ "orth")
    <*> (isJust <$> optional (first $ node $ hasAttrVal "name" "nps"))
    <*> first (hasAttrVal "name" "interps" //> lexQ)
    <*> first choiceQ )

lexQ :: Q (Lex L.Text)
lexQ = (hasAttrVal "type" "lex" *> attr "xml:id") `join` \_lexID -> ( Lex
    <$> pure _lexID
    <*> first (fStrQ "base")
    <*> first (fSymQ "ctag")
    <*> first (hasAttrVal "name" "msd" //> node
        ((,) <$> (named "symbol" *> attr "xml:id") <*> attr "value")) )

choiceQ :: Q (L.Text, L.Text)
choiceQ = hasAttrVal "name" "disamb" `joinR` ( first $ named "fs" `joinR` do
    ptr    <- first $ L.tail <$> node (attr "fVal")
    interp <- first $ fStrQ "interpretation"
    return (ptr, interp) )

-- | Parse textual contents of the ann_morphosyntax.xml file.
parseMorph :: L.Text -> [Para L.Text]
parseMorph =
    F.concat . evalP morphP . parseForest . TagSoup.parseTags

-- | Parse the stand-alone ann_morphosyntax.xml file.
readMorph :: FilePath -> IO [Para L.Text]
readMorph morphPath = parseMorph <$> L.readFile morphPath

-- | Parse ann_morphosyntax.xml files from the NCP corpus.
-- If the given list of directories is empty, all ann_morphosyntax.xml
-- files will be read.
readCorpus :: [FilePath] -> FilePath -> IO [(FilePath, Maybe [Para L.Text])]
readCorpus xs = C.readDirs xs "ann_morphosyntax.xml" parseMorph






-----------------------------------
-- BACKUP
-----------------------------------





-- -- | TEI NKJP ann_morphosyntax parser.
-- type P a = XmlParser L.Text a

-- morphP :: P [Para L.Text]
-- morphP = true //> paraP

-- paraP :: P (Para L.Text)
-- paraP = uncurry Para <$> (tag "p" *> getAttr "xml:id" </> sentP)

-- sentP :: P (Sent L.Text)
-- sentP = uncurry Sent <$> (tag "s" *> getAttr "xml:id" </> segP)

-- segP :: P (Seg L.Text)
-- segP = (tag "seg" *> getAttr "xml:id") `join` smP

-- smP :: L.Text -> P (Seg L.Text)
-- smP _segID = (tag "fs" *> hasAttr "type" "morph") `joinR` ( Seg
--     <$> pure _segID
--     <*> fStrP "orth"
--     <*> (isJust <$> (optional . cut $ hasAttr "name" "nps"))
--     <*> (hasAttr "name" "interps" /> lexP)
--     <*> choiceP )

-- lexP :: P (Lex L.Text)
-- lexP = (hasAttr "type" "lex" *> getAttr "xml:id") `join` \_lexID -> ( Lex
--     <$> pure _lexID
--     <*> fStrP "base"
--     <*> fSymP "ctag"
--     <*> (hasAttr "name" "msd" //> cut
--         ((,) <$> (tag "symbol" *> getAttr "xml:id") <*> getAttr "value")) )

-- choiceP :: P (L.Text, L.Text)
-- choiceP = hasAttr "name" "disamb" `joinR` ( tag "fs" `joinR` do
--     ptr <- L.tail <$> cut (getAttr "fVal")
--     interp <- fStrP "interpretation"
--     return (ptr, interp) )

-- fStrP :: L.Text -> P L.Text
-- fStrP x =
--     let checkName = tag "f" *> hasAttr "name" x
--         -- | Body sometimes is empty.
--         safeHead [] = ""
--         safeHead xs = head xs
--     in  safeHead <$> (checkName #> tag "string" /> text)

-- fSymP :: L.Text -> P L.Text
-- fSymP x =
--     let checkName = tag "f" *> hasAttr "name" x
--         p = cut (tag "symbol" *> getAttr "value")
--     in  head <$> (checkName /> p)

-- -- | Parse textual contents of the ann_morphosyntax.xml file.
-- parseMorph :: L.Text -> [Para L.Text]
-- parseMorph = parseXml morphP

