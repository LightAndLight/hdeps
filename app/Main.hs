{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Options.Applicative as Options
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), (.:?))
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Aeson as Json
import Data.String (fromString)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing)
import qualified System.Process as Process
import Data.Maybe (maybeToList, fromMaybe)

data Cli
  = Cli{ inputFile :: FilePath, outputDir :: FilePath }

cliParser :: Options.Parser Cli
cliParser =
  Cli <$>
    Options.strOption (Options.short 'i' <> Options.long "input" <> Options.metavar "FILE" <> Options.help "Input file" <> Options.value "./hdeps.json" <> Options.showDefault) <*>
    Options.strOption (Options.short 'o' <> Options.long "output" <> Options.metavar "DIRECTORY" <> Options.help "Output directory" <> Options.value "./hdeps" <> Options.showDefault)

newtype Hdeps
  = Hdeps (Map Text Hdep)
  deriving (FromJSON, ToJSON)

data Hdep
  = Hackage{ version :: !Text }
  | Github{ owner :: !Text, repository :: !Text, commit :: !Text, directory :: !(Maybe Text), tests :: !(Maybe Bool) }

instance ToJSON Hdep where
  toJSON (Hackage version) =
    Json.object [fromString "type" .= "hackage", fromString "version" .= version]
  toJSON (Github owner repository commit mDirectory mTests) =
    Json.object $
      [ fromString "type" .= "github"
      , fromString "owner" .= owner
      , fromString "repository" .= repository
      , fromString "commit" .= commit
      ] ++
      [ fromString "directory" .= directory | directory <- maybeToList mDirectory ] ++
      [ fromString "tests" .= tests | tests <- maybeToList mTests ]

instance FromJSON Hdep where
  parseJSON = Json.withObject "Hdep" $ \obj -> do
    type_ <- obj .: fromString "type"
    case type_ of
      "hackage" -> do
        version <- obj .: fromString "version"
        pure Hackage{ version }
      "github" -> do
        owner <- obj .: fromString "owner"
        repository <- obj .: fromString "repository"
        commit <- obj .: fromString "commit"
        directory <- obj .:? fromString "directory"
        tests <- obj .:? fromString "tests "
        pure Github{ owner, repository, commit, directory, tests }
      _ -> fail $ "invalid type: " ++ type_

main :: IO ()
main = do
  cli <- Options.execParser (Options.info cliParser Options.fullDesc)

  result <- Json.eitherDecodeFileStrict' @Hdeps cli.inputFile
  Hdeps hdeps <-
    case result of
      Left err -> do
        hPutStrLn stderr $ "error: failed to decode " ++ cli.inputFile ++ ": " ++ err
        exitFailure
      Right a -> pure a

  createDirectoryIfMissing True cli.outputDir
  drvFiles <-
    Map.traverseWithKey
      (\name hdep -> do
        drvFile <- getHdep cli.outputDir name hdep
        pure (name, drvFile)
      )
      hdeps

  let overlayFile = cli.outputDir ++ "/overlay.nix"
  writeFile overlayFile . unlines $
    "self: super: {" :
    foldMap
      (\(name, drvFile) ->
        ["  " ++ Text.unpack name ++ " = " ++ "self.callPackage ./" ++ Text.unpack drvFile ++ " {};"])
        drvFiles ++
    ["}"]
  hPutStrLn stderr $ "created " ++ overlayFile

readProcess :: FilePath -> [String] -> String -> IO String
readProcess =
  -- TODO: better error for missing program
  Process.readProcess

getHdep ::
  -- | Output directory
  FilePath ->
  -- | Package name
  Text ->
  Hdep ->
  -- | Derivation path relative to output directory
  IO Text
getHdep outputDir name (Hackage version) = do
  let
    url =
      "https://hackage.haskell.org/package/" <>
      Text.unpack name <>
      "/" <>
      Text.unpack name <>
      "-" <>
      Text.unpack version <>
      ".tar.gz"

  hPutStrLn stderr $ "fetching " ++ url ++ "..."
  let drvName = "hackage.haskell.org-" <> Text.unpack name <> "-" <> Text.unpack version
  let srcDrvName = drvName <> "-src"
  (hash, src) <- do
    output <- readProcess "nix-prefetch-url" ["--unpack" , "--print-path" , "--name" , srcDrvName , url] ""

    case lines output of
      hash : src : _ -> pure (hash, src)
      _ -> do
        hPutStrLn stderr $ "error: unexpected output from nix-prefetch-url: " ++ output
        exitFailure

  let srcDrvFileRelative = srcDrvName ++ ".nix"
  let srcDrvFile = outputDir ++ "/" ++ srcDrvFileRelative
  writeFile srcDrvFile $
    unlines
    [ "builtins.fetchTarball {"
    , "  url = \"" <> url <> "\";"
    , "  sha256 = \"" <> hash <> "\";"
    , "}"
    ]
  hPutStrLn stderr $ "  created " ++ srcDrvFile

  let sedEscape = concatMap (\c -> if c `elem` "/." then ['\\', c] else [c])

  let drvFileRelative = drvName ++ ".nix"
  let drvFile = outputDir ++ "/" ++ drvFileRelative
  writeFile drvFile =<<
    readProcess "sed" ["s/" ++ sedEscape src ++ "/" ++ sedEscape ("import ./" ++ srcDrvFileRelative) ++ "/g"] =<<
    readProcess "cabal2nix" [src] ""
  hPutStrLn stderr $ "  created " ++ drvFile

  hPutStrLn stderr "done"
  pure $ Text.pack drvFileRelative
getHdep outputDir _name (Github owner repository commit mDirectory mTests) = do
  let
    url =
      "https://github.com/" <>
      Text.unpack owner <>
      "/" <>
      Text.unpack repository <>
      "/archive/" <>
      Text.unpack commit <>
      ".tar.gz"

  hPutStrLn stderr $ "fetching " ++ url ++ "..."
  let drvName = "github.com-" <> Text.unpack owner <> "-" <> Text.unpack repository <> "-" <> Text.unpack commit <> foldMap (("-" <>) . Text.unpack) mDirectory
  let srcDrvName = drvName <> "-src"
  (hash, src) <- do
    output <- readProcess "nix-prefetch-url" ["--unpack" , "--print-path" , "--name" , srcDrvName , url] ""

    case lines output of
      hash : src : _ -> pure (hash, src)
      _ -> do
        hPutStrLn stderr $ "error: unexpected output from nix-prefetch-url: " ++ output
        exitFailure

  let srcDrvFileRelative = srcDrvName ++ ".nix"
  let srcDrvFile = outputDir ++ "/" ++ srcDrvFileRelative
  writeFile srcDrvFile $
    unlines
    [ "builtins.fetchTarball {"
    , "  url = \"" <> url <> "\";"
    , "  sha256 = \"" <> hash <> "\";"
    , "}"
    ]
  hPutStrLn stderr $ "  created " ++ srcDrvFile

  let sedEscape = concatMap (\c -> if c `elem` "/." then ['\\', c] else [c])

  let drvFileRelative = drvName ++ ".nix"
  let drvFile = outputDir ++ "/" ++ drvFileRelative
  writeFile drvFile =<<
    readProcess "sed" ["s/" ++ sedEscape src ++ "/" ++ sedEscape ("import ./" ++ srcDrvFileRelative) ++ "/g"] =<<
    readProcess
      "cabal2nix"
      (maybe [] (\directory -> ["--subpath", Text.unpack directory]) mDirectory ++
        [ "--no-check" | fromMaybe True mTests ] ++
        [src]) ""
  hPutStrLn stderr $ "  created " ++ drvFile

  hPutStrLn stderr "done"
  pure $ Text.pack drvFileRelative
