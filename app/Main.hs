{-# options_ghc -Wno-unused-imports #-}
module Main where

import Options.Applicative

import YAGPV (draw)

-- main :: IO ()
-- main = putStrLn "hello!"

main :: IO ()
main = do
  (Opts fpath) <- customExecParser pprefs opts
  draw fpath
  where
    opts = info (optsP <**> helper) (fullDesc <> progDesc "" <> header "yagpv")
    pprefs = prefs showHelpOnEmpty


optsP = Opts <$>
  strOption (
    long "file-in" <>
    short 'i' <>
    help "path of input file " <>
    metavar "FILEPATH"
    )

data Opts = Opts {
  oFileIn :: FilePath
                 }


-- main = greet =<< execParser opts
--   where
--     opts = info (sample <**> helper)
--       ( fullDesc
--      <> progDesc "Print a greeting for TARGET"
--      <> header "hello - a test for optparse-applicative" )
