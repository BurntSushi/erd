module Config
  ( Config(..)
  , configIO
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.Console.GetOpt as O
import System.IO (Handle, IOMode(..), stdin, stdout, stderr, openFile)
import Text.Printf (HPrintfType, hPrintf)

data Config =
  Config { cin :: (String, Handle)
         , cout :: (String, Handle)
         , cquiet :: Bool
         }

defaultConfig :: Config
defaultConfig =
  Config { cin = ("<stdin>", stdin)
         , cout = ("<stdout>", stdout)
         , cquiet = False
         }

configIO :: IO Config
configIO = do
  args <- getArgs
  case O.getOpt O.Permute opts args of
    (flags, [], []) -> foldl (\c app -> app c) (return defaultConfig) flags
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
       ++ "Otherwise, stdout will be used.")
  , O.Option "q" ["quiet"]
      (O.NoArg $ fmap (\c -> c { cquiet = True }))
      "When set, all warnings will be suppressed."
  , O.Option "h" ["help"]
      (O.NoArg $ const usageExit)
      "Show this usage message."
  ]

usageExit :: IO a
usageExit = usage >> exitFailure

usage :: IO a
usage = ef "%s\n" $ O.usageInfo "Usage: erd [flags]" opts

ef :: HPrintfType r => String -> r
ef = hPrintf stderr

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
