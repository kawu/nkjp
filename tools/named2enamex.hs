{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad (forM_, (>=>))
import           System.Environment (getArgs)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Data.Named.Tree
import Text.Named.Enamex (showForest)
import qualified Text.NKJP.Named as Ne
import qualified Text.NKJP.Morphosyntax as Mx

-- | A hard-coded flag: print one paragraph per line (`True`) or one sentence
-- per line (`False`)?
byPar :: Bool
byPar = True

neType :: Ne.NE L.Text -> T.Text
neType ne = L.toStrict . L.intercalate "." . catMaybes . map ($ ne) $
    [ Just . Ne.neType
    , Ne.subType
    , Ne.derived >=> return . Ne.derivType ]

orth :: [Mx.Seg L.Text] -> T.Text
orth = L.toStrict . L.concat . map Mx.orth

main :: IO ()
main = do
    [teiPath] <- getArgs
    pars <- concat <$> Ne.readTrees [] teiPath
    -- below, each paragraph is a list of forests
    forM_ pars $ \fs ->
      if byPar
      then L.putStrLn . showForest . prepare . concat $ fs
      else do
          forM_ fs $ \ts ->
              L.putStrLn . showForest . prepare $ ts
  where
    prepare
        = mapForest (onEither neType orth)
        . groupForestLeaves sndNps
    sndNps _ seg = Mx.nps seg
