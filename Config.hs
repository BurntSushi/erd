module Config
  ( Config(..)
  , configIO
  )
where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.Console.GetOpt as O
import System.IO (Handle, stdin, stdout, stderr)
import Text.Printf (HPrintfType, hPrintf)

data Config =
  Config { cin :: Handle
         , cout :: Handle
         , cquiet :: Bool
         }

defaultConfig :: Handle -- ^ The input file handle.
                 -> Handle -- ^ The output file handle.
                 -> Config
defaultConfig i o =
  Config { cin = i
         , cout = o
         , cquiet = False
         }

configIO :: IO Config
configIO = do
  args <- getArgs
  case O.getOpt O.Permute opts args of
    (_, _, errs@(_:_)) -> do
      ef "Error(s) parsing flags:\n\t%s" (intercalate "\n\t" errs)
      exitFailure
    (flags, _, []) ->
      foldl (\c app -> app c) (return $ defaultConfig stdin stdout) flags

opts :: [O.OptDescr (IO Config -> IO Config)]
opts =
  [ O.Option "q" ["quiet"]
      (O.NoArg $ fmap (\c -> c { cquiet = True }))
      "When set, all warnings will be suppressed."
  , O.Option "h" ["help"]
      (O.NoArg $ const usageExit)
      "Show this usage message."
  ]

usageExit :: IO a
usageExit = usage >> exitFailure

usage :: IO a
usage = ef "%s\n" $ O.usageInfo "Usage: erd [in-file] out-file" opts

ef :: HPrintfType r => String -> r
ef = hPrintf stderr
