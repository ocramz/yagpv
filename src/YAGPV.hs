{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module YAGPV where

-- containers
import Data.Tree (Tree(..), drawTree, Forest)
-- ghc-prof
import qualified GHC.Prof as P (Profile(..), decode, CostCentre(..), costCentres )
-- scientific
import Data.Scientific (Scientific)
-- text
import qualified Data.Text as T (Text, splitOn)
import qualified Data.Text.Lazy as TL (Text, fromStrict, toStrict)
import qualified Data.Text.Lazy.IO as TL (readFile)


data CCSummary = CCS {
    ccsName :: T.Text
  , ccsModule :: T.Text
  , ccsEntries :: !Integer
  , ccsInhTime :: Scientific
                     } deriving (Eq, Show)

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
