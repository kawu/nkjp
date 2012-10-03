{-# LANGUAGE OverloadedStrings #-}

-- | Parsing the NKJP named entity layer.

module Text.NKJP.Named
( parseNamed
, readNamed
, readCorpus
, module Data.NKJP.Named
) where

import System.FilePath (takeBaseName)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Text.XML.PolySoup
import Data.NKJP.Named

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

-- | Parse the NCP .tar.gz file.
readCorpus :: FilePath -> IO [(FilePath, [Para L.Text])]
readCorpus tarPath = do
    map parseEntry . withBase "ann_named" <$> readTar tarPath

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
withBase baseName = filter ((==baseName) . takeBaseName . Tar.entryPath)
