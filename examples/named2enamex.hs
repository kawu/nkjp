import Control.Monad (forM_)
import System.Environment (getArgs)
import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.Tree as T

import Data.NKJP.Named (mkForest)
import Data.Named.Tree (mapTrees)
import qualified Text.NKJP.Named as N
import qualified Text.NKJP.Morphosyntax as M

simple :: Either (N.NE t) (M.Seg t) -> Either t t
simple (Left ne) = Left (N.neType ne)
simple (Right seg) = Right (M.orth seg)

main = do
    [teiPath] <- getArgs

    named <- N.readCorpus teiPath
    morph <- M.readCorpus teiPath

    forM_ (zip morph named) $ \((path, mpara), (path', npara)) -> do
        putStrLn $ "### " ++ path
        putStrLn $ "### " ++ path'

        let msents = concatMap M.sentences mpara
            nsents = concatMap N.sentences npara

        forM_ (zip msents nsents) $ \(msent, nsent) -> do
            let forest = mkForest (M.segments msent) (N.names nsent)
            putStrLn . T.drawForest . mapTrees (show.simple) $ forest
