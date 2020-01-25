{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Erd.Config
  ( Config(..)
  , configIO
  , defaultConfig
  , defaultConfigFile
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
import           Data.Yaml                         (FromJSON (..), (.:))
import qualified Data.Yaml                         as Y
import qualified System.Console.GetOpt             as O
import           System.Directory                  (getHomeDirectory)
import           System.Environment                (getArgs)
import           System.Exit                       (exitFailure)
import           System.FilePath                   ((</>))
import           System.IO                         (Handle, IOMode (..),
                                                    openFile, stderr, stdin,
                                                    stdout)
import           System.IO.Error                   (isDoesNotExistError)
import           Text.Printf                       (HPrintfType, hPrintf,
                                                    printf)
import           Text.RawString.QQ

-- | Config represents all information from command line flags.
data Config =
  Config { cin        :: (String, Handle)
         , cout       :: (String, Handle)
         , outfmt     :: Maybe G.GraphvizOutput
         , edgeType   :: Maybe A.EdgeType
         , configFile :: Maybe FilePath
         , dotentity  :: Bool
         }

-- | Represents fields that are stored in the configuration file.
data ConfigFile = ConfigFile
  { cFmtOut    :: String
  , cEdgeType  :: String
  , cDotEntity :: Bool
  } deriving Show

instance FromJSON ConfigFile where
  parseJSON (Y.Object v) =
    ConfigFile <$>
    v .: "output-format" <*>
    v .: "edge-style" <*>
    v .: "dot-entity"
  parseJSON _ = fail "Incorrect configuration file."

defaultConfig :: Config
defaultConfig =
  Config { cin = ("<stdin>", stdin)
         , cout = ("<stdout>", stdout)
         , outfmt = Nothing
         , edgeType = Just A.SplineEdges
         , configFile = Nothing
         , dotentity = False
         }

defaultConfigFile :: B.ByteString
defaultConfigFile = B.unlines
  [[r|# Erd (~/.erd.yaml) default configuration file.|],
   B.append [r|output-format: pdf           # Supported formats: |] (defVals fmts),
   B.append [r|edge-style: spline           # Supported values : |] (defVals edges),
   B.append [r|dot-entity: false            # Supported values : |] (defVals valBool)
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
                        ef "'%s' is not a valid output format." fmt
                        exitFailure
                      Just gfmt -> return c {outfmt = Just gfmt}
                )
                "FMT")
      (printf "Force the output format to one of:\n%s"
              (intercalate ", " $ M.keys fmts))
  , O.Option "e" ["edge"]
      (O.ReqArg (\edge cIO -> do
                    c <- cIO
                    let edgeG = toEdgeG edge
                    case edgeG of
                      Nothing -> do
                        ef "'%s' is not a valid type of edge." edge
                        exitFailure
                      Just x -> return c {edgeType = Just x}
                )
                "EDGE")
      (printf "Select one type of edge:\n%s"
              (intercalate ", " $ M.keys edges))
  , O.Option "d" ["dot-entity"]
      (O.NoArg (\cIO -> do
                    c <- cIO
                    return $ c { dotentity = True } ))
      ("When set, output will consist of regular dot tables instead of HTML tables.\n"
      ++ "Formatting will be disabled. Use only for further manual configuration.")
  ]

toConfig :: ConfigFile -> Config
toConfig c = defaultConfig {outfmt    = toGraphFmt $ cFmtOut c,
                            edgeType  = toEdgeG $ cEdgeType c,
                            dotentity = cDotEntity c }

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
fmts :: M.Map String (Maybe G.GraphvizOutput)
fmts = M.fromList
  [ ("pdf", Just G.Pdf)
  , ("svg", Just G.Svg)
  , ("eps", Just G.Eps)
  , ("bmp", Just G.Bmp)
  , ("jpg", Just G.Jpeg)
  , ("png", Just G.Png)
  , ("gif", Just G.Gif)
  , ("tiff", Just G.Tiff)
  , ("dot", Just G.Canon)
  , ("ps",  Just G.Ps)
  , ("ps2", Just G.Ps2)
  , ("plain", Just G.Plain)
  ]

edges :: M.Map String (Maybe A.EdgeType)
edges = M.fromList
  [ ("spline", Just A.SplineEdges)
  , ("ortho", Just A.Ortho)
  , ("noedge", Just A.NoEdges)
  , ("poly", Just A.PolyLine)
  , ("compound", Just A.CompoundEdge)
  ]

valBool :: M.Map String Bool
valBool = M.fromList
  [ ("true", True)
  , ("false", False) ]

-- | takeExtension returns the last extension from a file path, or the
-- empty string if no extension was found. e.g., the extension of
-- "wat.pdf" is "pdf".
takeExtension :: String -> String
takeExtension s = if null rest then "" else reverse ext
  where (ext, rest) = span (/= '.') $ reverse s

toGraphFmt :: String -> Maybe G.GraphvizOutput
toGraphFmt ext = M.findWithDefault Nothing ext fmts

toEdgeG :: String -> Maybe A.EdgeType
toEdgeG edge = M.findWithDefault Nothing edge edges

usageExit :: IO a
usageExit = usage >> exitFailure

usage :: IO ()
usage = ef "%s\n" $ O.usageInfo "Usage: erd [flags]" opts

ef :: HPrintfType r => String -> r
ef = hPrintf stderr

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
