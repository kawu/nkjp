{-# LANGUAGE DeriveFunctor #-}

module Data.NKJP.Morphosyntax
( Para (..)
, Sent (..)
, Seg (..)
, Lex (..)
) where

-- | Paragraph.
data Para t = Para
    { paraID    :: t
    , sentences :: [Sent t] }
    deriving (Show, Functor)

-- | Sentence.
data Sent t = Sent
    { sentID    :: t
    , segments  :: [Seg t] }
    deriving (Show, Functor)

-- | Segment.
data Seg t = Seg
    { segID     :: t 
    , orth      :: t
    , nps       :: Bool
    , lexs      :: [Lex t]
    , choice    :: (t, t) }
    deriving (Show, Functor)

-- | Lexciacal entry -- possible interpretation.
data Lex t = Lex
    { lexID     :: t
    , base      :: t
    , ctag      :: t
    , msds      :: [(t, t)] }
    deriving (Show, Functor)
