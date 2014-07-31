{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Common types.


module Text.NKJP.Ptr
( Ptr (..)
, showPtr
, readPtr
) where


-- import           Control.Applicative
import qualified Data.Text.Lazy as L

-- import           Text.XML.PolySoup hiding (P, Q)
-- import qualified Text.XML.PolySoup as P


-- | A pointer.
data Ptr t
    -- | Of #target form.
    = Local
        { target    :: t }
    -- | Of location#target form.
    | Global
        { target    :: t
        , location  :: t }
    deriving (Show, Functor)


-- | Show pointer.
showPtr :: Ptr L.Text -> L.Text
showPtr (Local x) = x
showPtr (Global x y) = L.concat [y, "#", x]


-- | Make pointer from a lazy text.
readPtr :: L.Text -> Ptr L.Text
readPtr x = case L.break (=='#') x of
    (ptr, "")   -> Local ptr
    (loc, ptr)  -> Global
        { location = loc
        , target = (L.tail ptr) }
