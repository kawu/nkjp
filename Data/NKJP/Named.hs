{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Data.NKJP.Named
( Cert (..)
, Ptr (..)
, Deriv (..)
, Para (..)
, Sent (..)
, NE (..)
, mkForest
) where

import Data.Named.Graph (toForest, mkGraph)
import Data.Named.Tree (mapTrees)
import Data.NKJP.Morphosyntax (Seg, segID)
import qualified Data.Map as M
import qualified Data.Tree as T

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

-- | Make NE forest from a segment list and a list of NEs, both lists
-- corresponding to the same sentence.
mkForest :: Ord t => [Seg t] -> [NE t] -> T.Forest (Either (NE t) (Seg t))
mkForest xs ns =
    mapTrees decode (toForest graph)
  where
    -- Position of segment ID
    pos  = (M.!) $ M.fromList (zip (map segID xs) [0..])
    -- Segment on the given position
    word = (M.!) $ M.fromList (zip [0..] xs)
    -- NE with given ID
    name = (M.!) $ M.fromList [(neID ne, ne) | ne <- ns]

    graph  = mkGraph (0, length xs - 1)
        [ ( neID ne
          , map resolve (ptrs ne) )
        | ne <- ns ]

    resolve (Local ptr)    = Left ptr
    resolve (Global ptr _) = Right (pos ptr)

    decode (Left neID) = Left (name neID)
    decode (Right k)   = Right (word k)
