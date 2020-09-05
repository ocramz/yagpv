{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module YAGPV where

import Data.String (IsString(..))
-- containers
import Data.Tree (Tree(..), drawTree, Forest, foldTree, unfoldTree)
-- ghc-prof
import qualified GHC.Prof as P (Profile(..), decode, CostCentre(..), costCentres )
-- scientific
import Data.Scientific (Scientific)
-- text
import qualified Data.Text as T (Text, splitOn, pack, unpack)
import qualified Data.Text.Lazy as TL (Text, fromStrict, toStrict)
import qualified Data.Text.Lazy.IO as TL (readFile)


data CCSummary = CCS {
    ccsName :: T.Text
  , ccsModule :: T.Text
  , ccsEntries :: !Integer
  , ccsInhTime :: Scientific
                     } deriving (Eq)
instance Show CCSummary where
  show (CCS n m e t) = unwords [T.unpack n, T.unpack m, show e, show t]

-- prune q t = filter q <$> t

-- prune q = foldTree (\l xs -> if q l then xs else Node l xs)

-- prune q = foldTree $ \ l xs ->
--   case q l of
--     True -> Node l xs
--     False -> Node l xs

ccSummary :: P.CostCentre -> CCSummary
ccSummary s = CCS name' mdl nents iht
  where
    mdl = P.costCentreModule s
    name' = last $ T.splitOn "." $ P.costCentreName s
    nents = P.costCentreEntries s
    iht = P.costCentreInhTime s

draw :: FilePath -> IO ()
draw fp = do
  tl <- TL.readFile fp
  case P.decode tl of
    Left e -> error e
    Right prof -> case P.costCentres prof of
      Just tree -> putStrLn $ drawTree (show . ccSummary <$> tree)
      Nothing -> pure ()
