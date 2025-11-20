{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Applicative (optional, (<**>))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json (parseMaybe)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString.Char8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Options.Applicative as Options
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process as Process

data Cli
  = Cli {inputFile :: Maybe FilePath, outputDir :: FilePath}

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> optional
      ( Options.strOption $
          Options.short 'i'
            <> Options.long "input"
            <> Options.metavar "FILE"
            <> Options.help "Input file (default: read from stdin)"
      )
    <*> Options.strOption
      ( Options.short 'o'
          <> Options.long "output"
          <> Options.metavar "DIRECTORY"
          <> Options.help "Output directory"
          <> Options.value "./hdeps"
          <> Options.showDefault
      )

newtype Hdeps
  = Hdeps (Map Text Hdep)
  deriving (FromJSON, ToJSON)

data Hdep
  = Hackage {version :: !Text, revision :: !(Maybe Text)}
  | Github
      { owner :: !Text
      , repository :: !Text
      , commit :: !Text
      , directory :: !(Maybe Text)
      , tests :: !(Maybe Bool)
      }
  | Git
      { url :: !FilePath
      , commit :: !Text
      , directory :: !(Maybe Text)
      , tests :: !(Maybe Bool)
      }

instance ToJSON Hdep where
  toJSON (Hackage version mRevision) =
    Json.object $
      [ fromString "type" .= "hackage"
      , fromString "version" .= version
      ]
        ++ [fromString "revision" .= revision | revision <- maybeToList mRevision]
  toJSON (Github owner repository commit mDirectory mTests) =
    Json.object $
      [ fromString "type" .= "github"
      , fromString "owner" .= owner
      , fromString "repository" .= repository
      , fromString "commit" .= commit
      ]
        ++ [fromString "directory" .= directory | directory <- maybeToList mDirectory]
        ++ [fromString "tests" .= tests | tests <- maybeToList mTests]
  toJSON (Git url commit mDirectory mTests) =
    Json.object $
      [ fromString "type" .= "git"
      , fromString "url" .= url
      , fromString "commit" .= commit
      ]
        ++ [fromString "directory" .= directory | directory <- maybeToList mDirectory]
        ++ [fromString "tests" .= tests | tests <- maybeToList mTests]

instance FromJSON Hdep where
  parseJSON = Json.withObject "Hdep" $ \obj -> do
    type_ <- obj .: fromString "type"
    case type_ of
      "hackage" -> do
        version <- obj .: fromString "version"
        revision <- obj .:? fromString "revision"
        pure Hackage{version, revision}
      "github" -> do
        owner <- obj .: fromString "owner"
        repository <- obj .: fromString "repository"
        commit <- obj .: fromString "commit"
        directory <- obj .:? fromString "directory"
        tests <- obj .:? fromString "tests"
        pure Github{owner, repository, commit, directory, tests}
      "git" -> do
        url <- obj .: fromString "url"
        commit <- obj .: fromString "commit"
        directory <- obj .:? fromString "directory"
        tests <- obj .:? fromString "tests"
        pure Git{url, commit, directory, tests}
      _ -> fail $ "invalid type: " ++ type_

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)

  result <-
    case cli.inputFile of
      Nothing -> Json.eitherDecodeStrict' @Hdeps <$> ByteString.getContents
      Just inputFile -> Json.eitherDecodeFileStrict' @Hdeps inputFile
  Hdeps hdeps <-
    case result of
      Left err -> do
        hPutStrLn stderr $ "error: failed to decode " ++ fromMaybe "input" cli.inputFile ++ ": " ++ err
        exitFailure
      Right a -> pure a

  createDirectoryIfMissing True cli.outputDir
  names <-
    Map.traverseWithKey
      ( \name hdep -> do
          getHdep cli.outputDir name hdep
          pure name
      )
      hdeps

  let overlayFile = cli.outputDir ++ "/overlay.nix"
  writeFile overlayFile . unlines $
    "self: super: {"
      : foldMap
        ( \name ->
            ["  " ++ Text.unpack name ++ " = " ++ "self.callPackage ./" ++ Text.unpack name ++ " {};"]
        )
        names
      ++ ["}"]
  hPutStrLn stderr $ "created " ++ overlayFile

readProcess :: FilePath -> [String] -> String -> IO String
readProcess =
  -- TODO: better error for missing program
  Process.readProcess

sedEscape :: String -> String
sedEscape = concatMap (\c -> if c `elem` "/." then ['\\', c] else [c])

newtype NixStorePath = NixStorePath {value :: String}

newtype Hash = Hash {value :: String}

nixPrefetchUrl ::
  -- | @--unpack@
  Bool ->
  -- | Name in the Nix store
  String ->
  -- | URL
  String ->
  IO (Hash, NixStorePath)
nixPrefetchUrl unpack name url = do
  output <-
    readProcess
      "nix-prefetch-url"
      (["--unpack" | unpack] ++ ["--print-path", "--name", name, url])
      ""

  case lines output of
    hash : src : _ -> pure (Hash hash, NixStorePath src)
    _ -> do
      hPutStrLn stderr $ "error: unexpected output from nix-prefetch-url: " ++ output
      exitFailure

fetchUrl ::
  -- | Output directory
  FilePath ->
  -- | URL
  String ->
  -- | Name in the Nix store
  String ->
  -- | Nix file name
  String ->
  IO NixStorePath
fetchUrl outputDir url nixName fileName = do
  hPutStrLn stderr $ "fetching " ++ url ++ "..."
  (hash, storePath) <- nixPrefetchUrl False nixName url

  let nixFile = outputDir ++ "/" ++ fileName
  writeFile nixFile $
    unlines
      [ "builtins.fetchurl {"
      , "  url = \"" <> url <> "\";"
      , "  sha256 = \"" <> hash.value <> "\";"
      , "}"
      ]
  hPutStrLn stderr $ "  created " ++ nixFile

  pure storePath

fetchTarball ::
  -- | Output directory
  FilePath ->
  -- | URL
  String ->
  -- | Name in the Nix store
  String ->
  -- | Nix file name
  String ->
  IO NixStorePath
fetchTarball outputDir url nixName fileName = do
  hPutStrLn stderr $ "fetching " ++ url ++ "..."
  (hash, storePath) <- nixPrefetchUrl True nixName url

  let nixFile = outputDir ++ "/" ++ fileName
  writeFile nixFile $
    unlines
      [ "builtins.fetchTarball {"
      , "  url = \"" <> url <> "\";"
      , "  sha256 = \"" <> hash.value <> "\";"
      , "}"
      ]
  hPutStrLn stderr $ "  created " ++ nixFile

  pure storePath

nixPrefetchGit ::
  -- | Name in the Nix store
  String ->
  -- | URL
  String ->
  -- | Commit
  String ->
  IO (Hash, NixStorePath)
nixPrefetchGit name url commit = do
  output <-
    readProcess
      "nix-prefetch-git"
      ["--name", name, "--rev", commit, url]
      ""

  let
    parser value = do
      hash <- value .: fromString "sha256"
      src <- value .: fromString "path"
      pure (Hash hash, NixStorePath src)

  case Json.parseMaybe parser =<< Json.decode' (LazyByteString.Char8.pack output) of
    Nothing -> do
      hPutStrLn stderr $ "error: unexpected output from nix-prefetch-git: " ++ output
      exitFailure
    Just a ->
      pure a -- (h(Hash hash, NixStorePath src)

fetchGit ::
  -- | Output directory
  FilePath ->
  -- | URL
  String ->
  -- | Commit
  String ->
  -- | Name in the Nix store
  String ->
  -- | Nix file name
  String ->
  IO NixStorePath
fetchGit outputDir url commit nixName fileName = do
  hPutStrLn stderr $ "fetching " ++ url ++ "..."
  (_hash, storePath) <- nixPrefetchGit nixName url commit

  let nixFile = outputDir ++ "/" ++ fileName
  writeFile nixFile $
    unlines
      [ "builtins.fetchGit {"
      , "  url = \"" <> url <> "\";"
      , "  rev = \"" <> commit <> "\";"
      , "}"
      ]
  hPutStrLn stderr $ "  created " ++ nixFile

  pure storePath

urlHackageRevision ::
  -- | Package name
  Text ->
  -- | Package version
  Text ->
  -- | Revision
  Text ->
  Text
urlHackageRevision name version revision =
  fromString "https://hackage.haskell.org/package/"
    <> name
    <> fromString "-"
    <> version
    <> fromString "/revision/"
    <> revision
    <> fromString ".cabal"

urlHackagePackage ::
  -- | Package name
  Text ->
  -- | Version
  Text ->
  Text
urlHackagePackage name version =
  fromString "https://hackage.haskell.org/package/"
    <> name
    <> fromString "/"
    <> name
    <> fromString "-"
    <> version
    <> fromString ".tar.gz"

urlGithubCommit ::
  -- | Owner
  Text ->
  -- | Repository
  Text ->
  -- | Commit
  Text ->
  Text
urlGithubCommit owner repository commit =
  fromString "https://github.com/"
    <> owner
    <> fromString "/"
    <> repository
    <> fromString "/archive/"
    <> commit
    <> fromString ".tar.gz"

getHdep ::
  -- | Output directory
  FilePath ->
  -- | Package name
  Text ->
  Hdep ->
  -- | Derivation path relative to output directory
  IO ()
getHdep outputDir name (Hackage version mRevision) = do
  let packageDir = outputDir ++ "/" ++ Text.unpack name
  createDirectoryIfMissing True packageDir

  mRevisionInfo <- for mRevision $ \revision -> do
    let url = urlHackageRevision name version revision

    let nixFile = "cabal.nix"
    revisedCabalStorePath <-
      fetchUrl
        packageDir
        (Text.unpack url)
        ("hackage.haskell.org-" <> Text.unpack name <> "-" <> Text.unpack version <> ".cabal")
        nixFile

    pure (revision, revisedCabalStorePath)

  let url = urlHackagePackage name version

  let drvName = "hackage.haskell.org-" <> Text.unpack name <> "-" <> Text.unpack version
  src <- fetchTarball packageDir (Text.unpack url) (drvName <> "-src") "src.nix"

  mRevisedSrc <-
    for mRevisionInfo $ \(revision, revisedCabalSrc) -> do
      let srcRevisedNix = packageDir ++ "/src-revised.nix"
      writeFile srcRevisedNix $
        unlines
          [ "{ stdenv }:"
          , "stdenv.mkDerivation {"
          , "  name = \"" ++ drvName <> "-r" <> Text.unpack revision <> "-src" ++ "\";"
          , "  src = import ./src.nix;"
          , "  installPhase = ''"
          , "    mkdir $out"
          , "    cp -R * $out/"
          , "    cp ${import ./cabal.nix} $out/" ++ Text.unpack name ++ ".cabal"
          , "  '';"
          , "}"
          ]

      hPutStrLn stderr $ "  created " ++ srcRevisedNix
      pure revisedCabalSrc

  let drvFile = packageDir ++ "/default.nix"
  case mRevisedSrc of
    Nothing ->
      writeFile drvFile
        =<< readProcess "sed" ["s/" ++ sedEscape src.value ++ "/" ++ sedEscape "import ./src.nix" ++ "/g"]
        =<< readProcess "cabal2nix" [src.value] ""
    Just revisedCabalStorePath ->
      writeFile drvFile
        =<< readProcess
          "sed"
          [ "s/editedCabalFile = .*/src = " ++ sedEscape "import ./src-revised.nix { inherit stdenv; }" ++ ";/g"
          ]
        =<< readProcess "sed" ["/^ *sha256.*/d"]
        =<< readProcess "cabal2nix" ["--extra-arguments", "stdenv", revisedCabalStorePath.value] ""
  hPutStrLn stderr $ "  created " ++ drvFile

  hPutStrLn stderr "done"
getHdep outputDir name (Github owner repository commit mDirectory mTests) = do
  let packageDir = outputDir ++ "/" ++ Text.unpack name
  createDirectoryIfMissing True packageDir

  let url = urlGithubCommit owner repository commit

  let drvName =
        "github.com-"
          <> Text.unpack owner
          <> "-"
          <> Text.unpack repository
          <> "-"
          <> Text.unpack commit
          <> foldMap (("-" <>) . Text.unpack) mDirectory
  src <- fetchTarball packageDir (Text.unpack url) (drvName <> "-src") "src.nix"

  let drvFile = packageDir ++ "/default.nix"
  writeFile drvFile
    =<< readProcess "sed" ["s/" ++ sedEscape src.value ++ "/" ++ sedEscape "import ./src.nix" ++ "/g"]
    =<< readProcess
      "cabal2nix"
      ( maybe [] (\directory -> ["--subpath", Text.unpack directory]) mDirectory
          ++ ["--no-check" | maybe False not mTests]
          ++ [src.value]
      )
      ""
  hPutStrLn stderr $ "  created " ++ drvFile

  hPutStrLn stderr "done"
getHdep outputDir name (Git url commit mDirectory mTests) = do
  let packageDir = outputDir ++ "/" ++ Text.unpack name
  createDirectoryIfMissing True packageDir

  let drvName =
        "local-"
          <> Text.unpack commit
          <> foldMap (("-" <>) . Text.unpack) mDirectory
  src <- fetchGit packageDir url (Text.unpack commit) (drvName <> "-src") "src.nix"

  let drvFile = packageDir ++ "/default.nix"
  writeFile drvFile
    =<< readProcess "sed" ["s/" ++ sedEscape src.value ++ "/" ++ sedEscape "import ./src.nix" ++ "/g"]
    =<< readProcess
      "cabal2nix"
      ( maybe [] (\directory -> ["--subpath", Text.unpack directory]) mDirectory
          ++ ["--no-check" | maybe False not mTests]
          ++ [src.value]
      )
      ""
  hPutStrLn stderr $ "  created " ++ drvFile

  hPutStrLn stderr "done"
