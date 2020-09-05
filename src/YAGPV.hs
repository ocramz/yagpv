{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-unused-matches #-}
module YAGPV where

import Data.String (IsString(..))

--alga
import Algebra.Graph (empty, vertex, connect, Graph)
import Algebra.Graph.ToGraph (ToGraph(..))
import Algebra.Graph.Export.Dot (Attribute(..), Style(..), export, exportViaShow)
-- containers
import Data.Tree (Tree(..), drawTree, Forest, foldTree, unfoldTree)
-- filepath
import System.FilePath (takeBaseName)
-- ghc-prof
import qualified GHC.Prof as P (Profile(..), decode, CostCentre(..), costCentres, CostCentreNo)
-- scientific
import Data.Scientific (Scientific)
-- text
import qualified Data.Text as T (Text, take, splitOn, pack, unpack)
import qualified Data.Text.Lazy as TL (Text, fromStrict, toStrict)
import qualified Data.Text.Lazy.IO as TL (readFile)




-- | Build a graph from a tree by connecting at each level
t2g :: Tree a -> Graph a
t2g = foldTree $ \x gs ->
  foldl (\acc g -> connect (vertex x) g `connect` acc) empty gs

-- | Build a graph from a tree by connecting at each level, if the current label satisfies a predicate
t2gIf :: (a -> Bool) -> Tree a -> Graph a
t2gIf q = foldTree $ \ x gs ->
  let
    insf acc g
      | q x = connect (vertex x) g `connect` acc
      | otherwise = acc
  in foldl insf empty gs



style :: [Char] -> Style CCSummary [Char]
style name = Style name [] attrs vas eas vn vattrs eattrs
  where
    attrs = []
    vas = ["shape" := "circle"]
    eas = mempty
    vn x = "v" <> show (ccsNo x)
    vattrs x = ["label" := T.unpack (ccsName x)]
    eattrs x y = []

-- style = Style
--     { graphName               = "Example"
--     , preamble                = ["  // This is an example", ""]
--     , graphAttributes         = ["label" := "Example", "labelloc" := "top"]
--     , defaultVertexAttributes = ["shape" := "circle"]
--     , defaultEdgeAttributes   = mempty
--     , vertexName              = \x   -> "v" ++ show x
--     , vertexAttributes        = \x   -> ["color" := "blue"   | odd x      ]
--     , edgeAttributes          = \x y -> ["style" := "dashed" | odd (x * y)] }


data CCSummary = CCS {
    ccsNo :: P.CostCentreNo
  , ccsName :: T.Text
  , ccsModule :: T.Text
  , ccsEntries :: !Integer
  , ccsInhTime :: Scientific
                     } deriving (Eq)
instance Show CCSummary where
  show (CCS i n m e t) = unwords [T.unpack n, T.unpack m, show e, show t, show i]
instance Ord CCSummary where
  (CCS _ _ _ i1 _) <= (CCS _ _ _ i2 _) = i1 <= i2


ccSummary :: P.CostCentre -> CCSummary
ccSummary s = CCS i name' mdl nents iht
  where
    i = P.costCentreNo s
    mdl = P.costCentreModule s
    name' = last $ T.splitOn "." $ P.costCentreName s
    nents = P.costCentreEntries s
    iht = P.costCentreInhTime s

draw :: FilePath -> IO ()
draw fp = do
  tl <- TL.readFile fp
  let fname = takeBaseName fp
  case P.decode tl of
    Left e -> error e
    -- Right prof -> print $ P.profileCostCentreTree prof
    Right prof -> case P.costCentres prof of
      Just tree -> putStrLn $ drawTree (show . ccSummary <$> tree)
    --   Just tree -> do
    --     let
    --       tree' = ccSummary <$> tree
    --       -- gr = t2g tree'
    --       -- gr = t2gIf (\c -> T.take 3 (ccsName c) /= "CAF") tree'
    --     putStrLn $ export (style fname) gr
      Nothing -> pure ()
