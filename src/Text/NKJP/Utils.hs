{-# LANGUAGE OverloadedStrings #-}

-- | NKJP parsing utilities.

module Text.NKJP.Utils
( P, Q
, fStrQ
, fSymQ
) where

import           Control.Applicative
import qualified Data.Text.Lazy as L

import           Text.XML.PolySoup hiding (P, Q)
import qualified Text.XML.PolySoup as P

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
