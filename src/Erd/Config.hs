{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Erd.Config
  ( Config(..)
  , configIO
  , defaultConfig
  , defaultConfigFile
  , Notation(..)
  )
where

import           Control.Exception                 (tryJust)
import           Control.Monad                     (guard)
import qualified Data.ByteString.Char8             as B
import           Data.Char                         (isSpace)
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Commands            as G
import           Data.List                         (dropWhileEnd, intercalate)
import qualified Data.Map                          as M
import           Data.Maybe                        (isNothing)
import           Data.Version                      (showVersion)
import           Data.Yaml                         (FromJSON (..), (.:?))
import qualified Data.Yaml                         as Y
import           Development.GitRev                (gitHash)
import           Paths_erd                         (version)
import qualified System.Console.GetOpt             as O
import           System.Directory                  (getHomeDirectory)
import           System.Environment                (getArgs)
import           System.Exit                       (exitFailure, exitSuccess)
import           System.FilePath                   ((</>))
import           System.IO                         (Handle, IOMode (..),
                                                    openFile, stderr, stdin,
                                                    stdout)
import           System.IO.Error                   (isDoesNotExistError)
import           Text.Printf                       (HPrintfType, hPrintf,
                                                    printf)
import           Text.RawString.QQ

-- | Notation style for relations.
data Notation = UML | IE
  deriving Show

-- | Config represents all information from command line flags.
data Config = Config
    { cin         :: (String, Handle)
    , cout        :: (String, Handle)
    , outfmt      :: Maybe G.GraphvizOutput
    , edgeType    :: Maybe A.EdgeType
    , configFile  :: Maybe FilePath
    , dotentity   :: Maybe Bool
    , edgePattern :: Maybe A.StyleName
    , notation    :: Maybe Notation
    }

-- | Represents fields that are stored in the configuration file.
data ConfigFile = ConfigFile
    { cFmtOut      :: Maybe String
    , cEdgeType    :: Maybe String
    , cDotEntity   :: Maybe Bool
    , cEdgePattern :: Maybe String
    , cNotation    :: Maybe String
    }
    deriving Show

-- | A ConfigFile with all fields initialized with Nothing.
emptyConfigFile :: ConfigFile
emptyConfigFile = ConfigFile Nothing Nothing Nothing Nothing Nothing

instance FromJSON ConfigFile where
  parseJSON (Y.Object v) =
    ConfigFile <$>
    v .:? "output-format" <*>
    v .:? "edge-style" <*>
    v .:? "dot-entity" <*>
    v .:? "edge-pattern" <*>
    v .:? "notation"
  parseJSON Y.Null = return emptyConfigFile
  parseJSON _ = fail "Incorrect configuration file."

defaultConfig :: Config
defaultConfig =
  Config { cin = ("<stdin>", stdin)
         , cout = ("<stdout>", stdout)
         , outfmt = Nothing
         , edgeType = Just A.SplineEdges
         , configFile = Nothing
         , dotentity = Just False
         , edgePattern = Just A.Dashed
         , notation = Just UML
         }

defaultConfigFile :: B.ByteString
defaultConfigFile = B.unlines
  [[r|# Erd (~/.erd.yaml) default configuration file.|],
   B.append [r|output-format: pdf           # Supported formats: |] (defVals fmts),
   B.append [r|edge-style: spline           # Supported values : |] (defVals edges),
   B.append [r|dot-entity: false            # Supported values : |] (defVals valBool),
   B.append [r|edge-pattern: dashed         # Supported values : |] (defVals edgePatterns),
   B.append [r|notation: uml                # Supported values : |] (defVals notations)
  ]
  where
    defVals = B.pack . unwords . M.keys

-- | Creates a new Config value from command line options.
-- If an output path is given and `--fmt` is omitted, then a format
-- will be inferred from the output path extension.
-- Failing all of that, PDF is used by default.
configIO :: IO Config
configIO = do
  args <- getArgs
  case O.getOpt O.Permute opts args of
    (flags, [], []) -> do
      conf <- foldl (\c app -> app c) (return defaultConfig) flags
      let outpath = fst (cout conf)
      return $
        if isNothing (outfmt conf) && outpath /= "<stdout>" then
          conf { outfmt = toGraphFmt $ takeExtension outpath }
        else
          conf
    (_, _, errs@(_:_)) -> do
      ef "Error(s) parsing flags:\n\t%s\n" $
        intercalate "\n\t" $ map strip errs
      exitFailure
    (_, _, []) -> do
      ef "erd does not have any positional arguments.\n\n"
      usageExit

-- | Order of processing command-line options is important to keep priority of
-- configuration sources, in terms of increasing precedence.
-- 1. Command-line options
-- 2. Configuration file of arbitrary path, e.g. '-c"/tmp/myconfig.yaml"'
-- 3. Configuration file: ~/.erd.yaml
opts :: [O.OptDescr (IO Config -> IO Config)]
opts =
  [ O.Option "c" ["config"]
      (O.OptArg (\mf cIO -> cIO >>= \c -> do
                      globConfFile <- readGlobalConfigFile
                      f <- readConfigFile mf
                      case (f, globConfFile) of
                        (Nothing, Nothing) ->      -- Config-file is desired, but unavailable.
                          B.putStr defaultConfigFile >> return c
                        (Nothing, Just x) -> -- Use global config-file from ~/.erd.yaml .
                          return $ toConfig x
                        (Just x, _) ->        -- Use user specified config-file.
                          return $ toConfig x
                    ) "FILE")
      "Configuration file."
  , O.Option "i" ["input"]
      (O.ReqArg (\fpath cIO -> do
                   c <- cIO
                   i <- openFile fpath ReadMode
                   return $ c {cin = (fpath, i)}
                )
                "FILE")
      ("When set, input will be read from the given file.\n"
       ++ "Otherwise, stdin will be used.")
  , O.Option "o" ["output"]
      (O.ReqArg (\fpath cIO -> do
                    c <- cIO
                    o <- openFile fpath WriteMode
                    return $ c {cout = (fpath, o)}
                )
                "FILE")
      ("When set, output will be written to the given file.\n"
       ++ "Otherwise, stdout will be used.\n"
       ++ "If given and if --fmt is omitted, then the format will be\n"
       ++ "guessed from the file extension.")
  , O.Option "h" ["help"]
      (O.NoArg $ const usageExit)
      "Show this usage message."
  , O.Option "f" ["fmt"]
      (O.ReqArg (\fmt cIO -> do
                    c <- cIO
                    let mfmt = toGraphFmt fmt
                    case mfmt of
                      Nothing -> do
                        ef "'%s' is not a valid output format.\n" fmt
                        exitFailure
                      Just gfmt -> return c {outfmt = Just gfmt}
                )
                "FMT")
      (descriptionWithValuesList "Force the output format to one of" fmts)
  , O.Option "e" ["edge"]
      (O.ReqArg (\edge cIO -> do
                    c <- cIO
                    let edgeG = toEdgeG edge
                    case edgeG of
                      Nothing -> do
                        ef "'%s' is not a valid type of edge.\n" edge
                        exitFailure
                      Just x -> return c {edgeType = Just x}
                )
                "EDGE")
      (descriptionWithValuesList "Select one type of edge" edges)
  , O.Option "p" ["edge-pattern"]
      (O.ReqArg (\epat cIO -> do
                    c <- cIO
                    case toEdgePattern epat of
                      Nothing -> do
                        ef "'%s' is not a valid type of edge pattern.\n" epat
                        exitFailure
                      Just x -> return c {edgePattern = Just x}
                )
                "PATTERN")
      (descriptionWithValuesList "Select one of the edge patterns" edgePatterns)
  , O.Option "n" ["notation"]
      (O.ReqArg (\nt cIO -> do
                    c <- cIO
                    case toNotation nt of
                      Nothing -> do
                        ef "'%s' is not a valid notation style.\n" nt
                        exitFailure
                      Just x -> return c {notation = Just x}
                )
                "NOTATION")
      (descriptionWithValuesList "Select one of the notation styles" notations)
  , O.Option "d" ["dot-entity"]
      (O.NoArg (\cIO -> do
                    c <- cIO
                    return $ c { dotentity = Just True } ))
      ("When set, output will consist of regular dot tables instead of HTML tables.\n"
      ++ "Formatting will be disabled. Use only for further manual configuration.")
  , O.Option "v" ["version"]
      (O.NoArg $ const erdVersion) "Shows version of application and revision code."
  ]
  where
    descriptionWithValuesList :: String -> M.Map String a -> String
    descriptionWithValuesList txt m = printf (txt <> ":\n%s.") (intercalate ", " $ M.keys m)

toConfig :: ConfigFile -> Config
toConfig c = defaultConfig {outfmt      = cFmtOut c >>= toGraphFmt,
                            edgeType    = cEdgeType c >>= toEdgeG,
                            dotentity   = cDotEntity c,
                            edgePattern = cEdgePattern c >>= toEdgePattern,
                            notation    = cNotation c >>= toNotation}

-- | Reads and parses configuration file at default location: ~/.erd.yaml
readGlobalConfigFile :: IO (Maybe ConfigFile)
readGlobalConfigFile = do
  mHome <- tryJust (guard . isDoesNotExistError) getHomeDirectory
  case mHome of
    Left _     -> return Nothing
    Right home -> readConfigFile $ Just (home </> ".erd.yaml")

-- | Reads and parses a configuration file, exceptions may come via
-- AesonException.
readConfigFile :: Maybe FilePath -> IO (Maybe ConfigFile)
readConfigFile Nothing = return Nothing
readConfigFile (Just f) = do
  mHome <- tryJust (guard . isDoesNotExistError) $ B.readFile f
  case mHome of
    Left _     -> return Nothing
    Right home -> Y.decodeThrow home

-- | A subset of formats supported from GraphViz.
fmts :: M.Map String G.GraphvizOutput
fmts = M.fromList
  [ ("pdf", G.Pdf)
  , ("svg", G.Svg)
  , ("eps", G.Eps)
  , ("bmp", G.Bmp)
  , ("jpg", G.Jpeg)
  , ("png", G.Png)
  , ("gif", G.Gif)
  , ("tiff", G.Tiff)
  , ("dot", G.Canon)
  , ("ps", G.Ps)
  , ("ps2", G.Ps2)
  , ("plain", G.Plain)
  ]

edges :: M.Map String A.EdgeType
edges = M.fromList
  [ ("spline", A.SplineEdges)
  , ("ortho", A.Ortho)
  , ("noedge", A.NoEdges)
  , ("poly", A.PolyLine)
  , ("compound", A.CompoundEdge)
  ]

valBool :: M.Map String Bool
valBool = M.fromList
  [ ("true", True)
  , ("false", False) ]

edgePatterns :: M.Map String A.StyleName
edgePatterns = M.fromList
  [ ("solid", A.Solid)
  , ("dashed", A.Dashed)
  , ("dotted", A.Dotted)
  ]

notations :: M.Map String Notation
notations = M.fromList
  [ ("uml", UML)
  , ("ie", IE)
  ]

-- | takeExtension returns the last extension from a file path, or the
-- empty string if no extension was found. e.g., the extension of
-- "wat.pdf" is "pdf".
takeExtension :: String -> String
takeExtension s = if null rest then "" else reverse ext
  where (ext, rest) = span (/= '.') $ reverse s

toGraphFmt :: String -> Maybe G.GraphvizOutput
toGraphFmt = (`M.lookup` fmts)

toEdgeG :: String -> Maybe A.EdgeType
toEdgeG = (`M.lookup` edges)

toEdgePattern :: String -> Maybe A.StyleName
toEdgePattern = (`M.lookup` edgePatterns)

toNotation :: String -> Maybe Notation
toNotation = (`M.lookup` notations)

usageExit :: IO a
usageExit = usage >> exitFailure

usage :: IO ()
usage = hPrintf stderr "%s\n" $ O.usageInfo "Usage: erd [flags]" opts

erdVersion :: IO a
erdVersion = do
  hPrintf stdout "erd-%s %s\n" (showVersion version) ($(gitHash) :: String)
  exitSuccess

ef :: HPrintfType r => String -> r
ef = hPrintf stderr

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
