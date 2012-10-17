{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parsing the NKJP named entity layer.

module Text.NKJP.Named
(
-- * Data types
  Cert (..)
, Ptr (..)
, Deriv (..)
, Para (..)
, Sent (..)
, NE (..)

-- * Parsing
, parseNamed
, readNamed
, readCorpus
, readTrees

-- * Utilities
, mkForest
) where

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Text.XML.PolySoup
import qualified Data.Named.Graph as Nd
import qualified Data.Named.Tree as Nd
import qualified Text.NKJP.Tar as Tar
import qualified Text.NKJP.Morphosyntax as Mx

-- | A certainty of an annotator.
data Cert
    = High
    | Medium
    | Low
    deriving (Show)

-- | A pointer.
data Ptr t
    -- | Of #id form.
    = Local
        { target    :: t }
    -- | Of loc#id form.
    | Global
        { target    :: t
        , location  :: t }
    deriving (Show, Functor)

-- | A derivation structure.
data Deriv t = Deriv
    { derivType :: t 
    , derivFrom :: t }
    deriving (Show, Functor)

-- | A paragraph.
data Para t = Para
    { paraID    :: t
    , sentences :: [Sent t] }
    deriving (Show, Functor)

-- | A sentence.
data Sent t = Sent
    { sentID    :: t
    , names     :: [NE t] }
    deriving (Show, Functor)

-- | A segment element in a file. 
data NE t = NE
    { neID          :: t
    , derived       :: Maybe (Deriv t)
    , neType        :: t
    , subType       :: Maybe t
    , orth          :: t
    -- | Left base or Right when.
    , base          :: Either t t
    , cert          :: Cert
    , certComment   :: Maybe t
    , ptrs          :: [Ptr t] }
    deriving (Show)

instance Functor NE where
    fmap f NE{..} = NE
        { neID          = f neID
        , derived       = fmap (fmap f) derived
        , neType        = f neType
        , subType       = fmap f subType
        , orth          = f orth
        , base          = case base of
            Left x  -> Left  (f x)
            Right x -> Right (f x)
        , cert          = cert
        , certComment   = fmap f certComment
        , ptrs          = map (fmap f) ptrs }

-- | Make NE forest from a segment list and a list of NEs, both lists
-- corresponding to the same sentence.
mkForest :: Ord t => [Mx.Seg t] -> [NE t]
         -> T.Forest (Either (NE t) (Mx.Seg t))
mkForest xs ns =
    Nd.mapForest decode (Nd.toForest graph)
  where
    -- Position of segment ID
    pos  = (M.!) $ M.fromList (zip (map Mx.segID xs) [0..])
    -- Segment on the given position
    word = (M.!) $ M.fromList (zip [0..] xs)
    -- NE with given ID
    name = (M.!) $ M.fromList [(neID ne, ne) | ne <- ns]

    graph  = Nd.mkGraph (0, length xs - 1)
        [ ( neID ne
          , map resolve (ptrs ne) )
        | ne <- ns ]

    resolve (Local ptr)    = Left ptr
    resolve (Global ptr _) = Right (pos ptr)

    decode (Left neID) = Left (name neID)
    decode (Right k)   = Right (word k)

-- | TEI NKJP ann_morphosyntax parser.
type P a = XmlParser L.Text a

namedP :: P [Para L.Text]
namedP = true //> paraP

paraP :: P (Para L.Text)
paraP = uncurry Para <$> (tag "p" *> getAttr "xml:id" </> sentP)

sentP :: P (Sent L.Text)
sentP = uncurry Sent <$> (tag "s" *> getAttr "xml:id" </> nameP)

nameP :: P (NE L.Text)
nameP = (tag "seg" *> getAttr "xml:id") `join` \_neID -> do
    ne <- nameBodyP
    _ptrs <- some namePtrP
         <|> failBad ("no targets specified for " ++ L.unpack _neID)
    return $ ne { neID = _neID, ptrs = _ptrs }

nameBodyP :: P (NE L.Text)
nameBodyP = (tag "fs" *> hasAttr "type" "named") `joinR` do
    _deriv   <- optional derivP
    _neType  <- fSymP "type"
    _subType <- optional (fSymP "subtype")
    _orth    <- fStrP "orth"
    _base    <- (Left  <$> fStrP "base") <|> (Right <$> fStrP "when")
    _cert    <- certP
    _certComment <- optional (fStrP "comment")
    return $ NE { neType = _neType, subType = _subType, orth = _orth
                , base = _base, derived = _deriv, cert = _cert
                , certComment = _certComment, neID = "", ptrs = [] }

derivP :: P (Deriv L.Text)
derivP = fP "derived" `joinR` ( fsP "derivation" `joinR` do
    Deriv <$> fSymP "derivType" <*> fStrP "derivedFrom" )
    
fP :: L.Text -> TagPred L.Text ()
fP x  = tag "f"  *> hasAttr "name" x

fsP :: L.Text -> TagPred L.Text ()
fsP x = tag "fs" *> hasAttr "type" x

certP :: P Cert
certP =
    mkCert <$> fSymP "certainty"
  where
    mkCert "high"   = High
    mkCert "medium" = Medium
    mkCert "low"    = Low
    mkCert _        = Medium    -- It should not happen!

namePtrP :: P (Ptr L.Text)
namePtrP = cut (tag "ptr" *> getAttr "target") >>= \x -> return $
    case L.break (=='#') x of
        (ptr, "")   -> Local ptr
        (loc, ptr)  -> Global
            { location = loc
            , target = (L.tail ptr) }

fStrP :: L.Text -> P L.Text
fStrP x =
    let checkName = tag "f" *> hasAttr "name" x
        -- Body sometimes is empty.
        safeHead [] = ""
        safeHead xs = head xs
    in  safeHead <$> (checkName #> tag "string" /> text)

fSymP :: L.Text -> P L.Text
fSymP x =
    let checkName = tag "f" *> hasAttr "name" x
        p = cut (tag "symbol" *> getAttr "value")
    in  head <$> (checkName /> p)

-- | Parse textual contents of the ann_named.xml file.
parseNamed :: L.Text -> [Para L.Text]
parseNamed = parseXml namedP

-- | Parse the stand-alone ann_named.xml file.
readNamed :: FilePath -> IO [Para L.Text]
readNamed namedPath = parseNamed <$> L.readFile namedPath

-- | Parse all ann_named.xml files from the NCP .tar.gz file.
readCorpus :: FilePath -> IO [(FilePath, Maybe [Para L.Text])]
readCorpus = Tar.readCorpus "ann_named" parseNamed

-- | Parse the NCP .tar.gz corpus, extract all NEs and translate them
-- to the tree form using the 'mkForest' function. 
readTrees :: FilePath -> IO [T.Forest (Either (NE L.Text) (Mx.Seg L.Text))]
readTrees path = do
    morph <- Mx.readCorpus path
    named <- readCorpus path
    return . concat $ map toTrees (sync morph named)
  where
    toTrees (_, xs, ys) = map toForest $ zip
        (concatMap Mx.sentences xs)
        (concatMap sentences ys)
    toForest (x, y) = mkForest (Mx.segments x) (names y)

sync :: [(FilePath, Maybe a)] -> [(FilePath, Maybe b)] -> [(FilePath, a, b)]
sync as bs =
    mapMaybe (uncurry just) (zip as bs)
  where
    just (dir, Just x) (_, Just y) = Just (dir, x, y)
    just _ _ = Nothing
