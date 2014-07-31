{-# LANGUAGE OverloadedStrings #-}

-- | NKJP parsing utilities.

module Text.NKJP.Utils
( P, Q
, fStrQ
, fSymQ
, idesQ
, idesQ'
) where

import           Control.Applicative
import qualified Data.Text.Lazy as L

import qualified Text.HTML.TagSoup as S
import           Text.XML.PolySoup hiding (P, Q)
import qualified Text.XML.PolySoup as P

import           Text.NKJP.Ptr

-- | TEI NKJP parsing predicates.
type P a = P.P (XmlTree L.Text) a
type Q a = P.Q (XmlTree L.Text) a

fStrQ :: L.Text -> Q L.Text
fStrQ x =
    let checkName = named "f" *> hasAttrVal "name" x
        -- | Body sometimes is empty.
        safeHead [] = ""
        safeHead (z:_) = z
    in  fmap safeHead $ checkName `joinR` do
            first $ named "string" /> node text

fSymQ :: L.Text -> Q L.Text
fSymQ x =
    let checkName = named "f" *> hasAttrVal "name" x
        p = named "symbol" *> attr "value"
        safeHead [] = error "fSymQ: empty head"
        safeHead (z:_) = z
    in  safeHead <$> (checkName /> node p)

-- | Identifier and corresp.
idesQ :: P.Q (S.Tag L.Text) (Ptr L.Text, L.Text)
idesQ = (,)
    <$> (readPtr <$> attr "corresp")
    <*> attr "xml:id"


-- | Identifier and corresp.  A provisional function.
idesQ' :: P.Q (S.Tag L.Text) (Maybe (Ptr L.Text), L.Text)
idesQ' = (,)
    <$> (optional (readPtr <$> attr "corresp"))
    <*> attr "xml:id"
