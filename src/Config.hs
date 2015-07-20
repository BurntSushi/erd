module Config
  ( Config(..)
  , configIO
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified System.Console.GetOpt as O
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (Handle, IOMode(..), stdin, stdout, stderr, openFile)
import Text.Printf (HPrintfType, hPrintf, printf)

import qualified Data.GraphViz.Commands as G
import qualified Data.GraphViz.Attributes.Complete as A

-- | Config represents all information from command line flags.
data Config =
  Config { cin :: (String, Handle)
         , cout :: (String, Handle)
         , outfmt :: Maybe G.GraphvizOutput
         , edgeType :: A.EdgeType
         }

defaultConfig :: Config
defaultConfig =
  Config { cin = ("<stdin>", stdin)
         , cout = ("<stdout>", stdout)
         , outfmt = Nothing
         , edgeType = A.SplineEdges
         }

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

opts :: [O.OptDescr (IO Config -> IO Config)]
opts =
  [ O.Option "i" ["input"]
      (O.ReqArg (\fpath cIO -> do
                   c <- cIO
                   i <- openFile fpath ReadMode
                   return $ c { cin = (fpath, i) }
                )
                "FILE")
      ("When set, input will be read from the given file.\n"
       ++ "Otherwise, stdin will be used.")
  , O.Option "o" ["output"]
      (O.ReqArg (\fpath cIO -> do
                    c <- cIO
                    o <- openFile fpath WriteMode
                    return $ c { cout = (fpath, o) }
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
                      Just gfmt ->
                        return $ c { outfmt = Just gfmt }
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
                      Just edgeType ->
                        return $ c { edgeType = edgeType }
                )
                "EDGE")
      (printf "Select one type of edge:\n%s"
              (intercalate ", " $ M.keys edges))
  ]

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
  , ("line", Just A.LineEdges)
  , ("noedge", Just A.NoEdges)
  , ("poly", Just A.PolyLine)
  , ("curved", Just A.Curved)
  , ("compound", Just A.CompoundEdge)
  ]


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
