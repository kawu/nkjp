{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Data.NKJP.Named
( Cert (..)
, Ptr (..)
, Deriv (..)
, Para (..)
, Sent (..)
, NE (..)
) where

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
