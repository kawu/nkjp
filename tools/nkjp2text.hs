{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad (forM_)
import           System.Environment (getArgs)
import           Data.Foldable (foldMap)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Text.NKJP.Named as Ne
import qualified Text.NKJP.Morphosyntax as Mx

main :: IO ()
main = do
    [teiPath] <- getArgs
    pars <- concat <$> Ne.readTrees [] teiPath
    forM_ pars $ \par ->
        L.putStrLn . L.strip . showLeaves . getLeaves . concat $ par
  where
    getLeaves  = concatMap (foldMap getRight)
    showLeaves = L.concat . map showLeaf
    showLeaf x = if Mx.nps x
        then Mx.orth x
        else " " `L.append` Mx.orth x
    getRight (Right x) = [x]
    getRight _         = []
