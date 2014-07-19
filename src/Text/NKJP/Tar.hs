module Text.NKJP.Tar
( readCorpus
) where

import Control.Applicative ((<$>), (<*>))
import System.FilePath (takeBaseName, takeDirectory)
import Data.List (groupBy, find)
import Data.Function (on)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
-- import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS

readTar :: FilePath -> IO [Tar.Entry]
readTar tar
    =  Tar.foldEntries (:) [] (error . show)
    .  Tar.read . BZip.decompress
   <$> BS.readFile tar

withBase :: String -> [Tar.Entry] -> Maybe Tar.Entry
withBase baseName = find ((==baseName) . takeBaseName . Tar.entryPath)

procContent :: (L.Text -> a) -> Tar.Entry -> a
procContent f entry =
    let (Tar.NormalFile binary _) = Tar.entryContent entry
    in  f (L.decodeUtf8 binary)

-- | Visit each .tar directory and return (apart from the directory name)
-- processed contents of the entry with the given name or Nothing if such
-- entry doesn't exists in the directory.
readCorpus :: String -> (L.Text -> a) -> FilePath -> IO [(FilePath, Maybe a)]
readCorpus base f tarPath
    = map onGroup
    . groupBy ((==) `on` (takeDirectory . Tar.entryPath))
   <$> readTar tarPath
  where
    onGroup = (,)
        <$> takeDirectory . Tar.entryPath . head
        <*> (fmap (procContent f) . withBase base)
