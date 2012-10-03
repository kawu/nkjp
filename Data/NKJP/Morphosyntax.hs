{-# LANGUAGE DeriveFunctor #-}

-- | Data types for the morphosytnax layer of the NKJP corpus.

module Data.NKJP.Morphosyntax
( Para (..)
, Sent (..)
, Seg (..)
, Lex (..)
) where

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
