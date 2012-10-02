{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Text.NKJP.Named
( Cert (..)
, Ptr (..)
, Deriv (..)
, Para (..)
, Sent (..)
, NE (..)
, parseNamed
, readNamed
, readCorpus
) where

import System.FilePath (takeBaseName)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Text.XML.PolySoup

-- | A certainty of an annotator.
data Cert
    = High
    | Medium
    | Low
    deriving (Show)

-- | A pointer.
data Ptr t
    -- | Of "#id" form.
    = Local
        { target    :: t }
    -- | Of "loc#id" form.
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

-- | TEI NKJP ann_morphosyntax parser.
type P a = XmlParser L.Text a

namedP :: P [Para L.Text]
namedP = true //> paraP

paraP :: P (Para L.Text)
paraP = uncurry Para <$> (tag "p" *> getAttr "xml:id" </> sentP)

sentP :: P (Sent L.Text)
sentP = uncurry Sent <$> (tag "s" *> getAttr "xml:id" </> nameP)

nameP :: P (NE L.Text)
nameP = (tag "seg" *> getAttr "xml:id") `join` \neID -> do
    ne <- nameBodyP
    ptrs <- some namePtrP
        <|> failBad ("no targets specified for " ++ L.unpack neID)
    return $ ne { neID = neID, ptrs = ptrs }

nameBodyP :: P (NE L.Text)
nameBodyP = (tag "fs" *> hasAttr "type" "named") `joinR` do
    deriv <- optional derivP
    neType <- fSymP "type"
    subType <- optional (fSymP "subtype")
    orth <- fStrP "orth"
    base <- (Left  <$> fStrP "base")
        <|> (Right <$> fStrP "when")
    cert <- certP
    certComment <- optional (fStrP "comment")
    return $ NE { neType = neType, subType = subType, orth = orth, base = base
                , derived = deriv, cert = cert, certComment = certComment
                , neID = "", ptrs = [] }    -- Should be supplied outside

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
    mkCert _        = Medium    -- ^ It should not happen!

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
        -- | Body sometimes is empty.
        safeHead [] = ""
        safeHead xs = head xs
    in  safeHead <$> (checkName #> tag "string" /> text)

fSymP :: L.Text -> P L.Text
fSymP x =
    let checkName = tag "f" *> hasAttr "name" x
        p = cut (tag "symbol" *> getAttr "value")
    in  head <$> (checkName /> p)

parseNamed :: L.Text -> [Para L.Text]
parseNamed = parseXml namedP

-- | NKJP .tar.gz handling.  TODO: Move NCP parsing utilities
-- to another, common module.  It should also allow parsing
-- plain directories.

-- | Parse the NCP .tar.gz file.
readCorpus :: FilePath -> IO [(FilePath, [Para L.Text])]
readCorpus tarPath = do
    map parseEntry . withBase "ann_named" <$> readTar tarPath

-- | Parse the stand-alone ann_named.xml file.
readNamed :: FilePath -> IO [Para L.Text]
readNamed namedPath = parseNamed <$> L.readFile namedPath

readTar :: FilePath -> IO [Tar.Entry]
readTar tar
    =  Tar.foldEntries (:) [] error
    .  Tar.read . GZip.decompress
   <$> BS.readFile tar

parseEntry :: Tar.Entry -> (FilePath, [Para L.Text])
parseEntry entry =
    (Tar.entryPath entry, parseNamed content)
  where
    (Tar.NormalFile binary _) = Tar.entryContent entry
    content = L.decodeUtf8 binary

withBase :: String -> [Tar.Entry] -> [Tar.Entry]
withBase base = filter ((==base) . takeBaseName . Tar.entryPath)
