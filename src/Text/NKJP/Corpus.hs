{-# LANGUAGE TupleSections #-}


module Text.NKJP.Corpus
( readCorpus
, readDirs
) where

import           Control.Applicative ((<$>))
import qualified Control.Monad.LazyIO as LazyIO
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L


-- | List all directories of the corpus. 
listCor :: FilePath -> IO [FilePath]
listCor path = do
    -- The structure of the NKJP corpus is flat. 
    filter (not . (`elem` [".", ".."])) <$> Dir.getDirectoryContents path


-- | Visit each directory and return (apart from the directory name)
-- processed contents of the entry with the given name or Nothing if such
-- entry doesn't exists in the directory.
readCorpus
    :: FilePath         -- ^ Element name, e.g. "ann_morphosyntax.xml"
    -> (L.Text -> a)    -- ^ Processing function
    -> FilePath         -- ^ Corpus directory
    -> IO [(FilePath, Maybe a)]
readCorpus elemName procElem corPath = do
    paths <- listCor corPath 
    doRead paths elemName procElem corPath


-- | Like `readCorpus`, but reads specified directories.
readDirs
    :: [FilePath]       -- ^ Directories to read
    -> FilePath         -- ^ Element name, e.g. "ann_morphosyntax.xml"
    -> (L.Text -> a)    -- ^ Processing function
    -> FilePath         -- ^ Corpus directory
    -> IO [(FilePath, Maybe a)]
readDirs [] = readCorpus
readDirs xs = doRead xs


-- | The function which actually does the reading.
doRead
    :: [FilePath]       -- ^ Directories to read
    -> FilePath         -- ^ Element name, e.g. "ann_morphosyntax.xml"
    -> (L.Text -> a)    -- ^ Processing function
    -> FilePath         -- ^ Corpus directory
    -> IO [(FilePath, Maybe a)]
doRead paths elemName procElem corPath = do
    LazyIO.forM paths $ \path -> do
        let filePath = corPath </> path </> elemName
        b <- Dir.doesFileExist filePath
        if b
            then (filePath,) . Just . procElem <$> L.readFile filePath
            else return (filePath, Nothing)
