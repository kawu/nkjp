{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_, (>=>))
import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Data.Named.Tree
import Text.Named.Enamex (showForest)
import qualified Text.NKJP.Named as Ne
import qualified Text.NKJP.Morphosyntax as Mx

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
    fs <- Ne.readTrees teiPath
    forM_ fs $ \ts ->
        L.putStrLn . showForest . prepare $ ts
  where
    prepare
        = mapForest (onEither neType orth)
        . groupForestLeaves sndNps
    sndNps _ seg = Mx.nps seg
