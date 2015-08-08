{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment

import Data.Aeson
import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

{- The trace event file format is documented at
   https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview
 -}

data Event = Event {
  cat :: T.Text,
  pid :: Int,
  tid :: Int,
  ts :: Int,
  ph :: T.Text,
  name :: T.Text,
  id :: Maybe T.Text,
  args :: Args
} deriving Generic

data File = File [Event] deriving Generic

unFile :: File -> [Event]
unFile (File rows) = rows

data Args = Args [(T.Text, Value)]

instance ToJSON Event
instance ToJSON File

instance ToJSON Args where
  toJSON (Args pairs) = object [ k .= v | (k,v) <- pairs]

type Deps = [Dep]
data Dep = Dep T.Text T.Text

nameOfDep :: Dep -> T.Text
nameOfDep (Dep p d) = T.unwords [p, "->", d]

getInput :: [String] -> IO (File, Deps)
getInput as =
  case as of
    []                  -> liftM2 go  T.getContents       (return "")
    [logFile]           -> liftM2 go (T.readFile logFile) (return "")
    [logFile, depsFile] -> liftM2 go (T.readFile logFile) (T.readFile depsFile)
    _ -> error "usage: $0 [<cabal build log> [<ghc-pkg dot>]]"
  where
    go f "" = (parseFile f, [])
    go f d  = (parseFile f, parseDeps d)

main :: IO ()
main = do
  (file, deps) <- getInput =<< getArgs
  let known_pkgs = Set.fromList . map name . unFile $ file 
      known_deps = filter (\(Dep p d) -> Set.member p known_pkgs && Set.member d known_pkgs) deps
  B.putStrLn . encode . process known_deps $ file

pidFixup :: File -> File
pidFixup (File rows) = File (go ([0..], []) rows)
  where
    go _ [] = []
    go (ns, ps) (r:rs) = r { tid = n } : r' : go (ns', ps') rs
      where
        r' = r { name = "parallel", pid = 1, ph = "C", args = Args [("", toJSON (length ps'))]}
        (n,ns',ps')
          | ph r == "B" = (head ns, tail ns, (name r, head ns):ps)
          | otherwise = fromMaybe (error ("tried to end pkg that has not started: " ++ T.unpack (name r))) $ do
              n' <- lookup (name r) ps
              return (n', n':ns, [ (k,v) | (k,v) <- ps, k /= name r])

flowFixup :: Deps -> File -> File
flowFixup deps = File . go . unFile
  where
    go [] = []
    go (r:rs) =
      case ph r of
        "B" -> iDependOn r ++ [r] ++ go rs
        "E" -> r : dependOnMe r ++ go rs
        _ -> r : go rs
    make r phase dep = r { ph = phase, name = nameOfDep dep, Main.id = Just (nameOfDep dep) }
    iDependOn r  = [ make r "f" dep | dep@(Dep p _) <- deps, name r == p ]
    dependOnMe r = [ make r "s" dep | dep@(Dep _ d) <- deps, name r == d ]

process :: Deps -> File -> File
process deps = flowFixup deps . pidFixup

parseFile :: T.Text -> File
parseFile = File . mapMaybe parseEvent . T.lines

parseEvent :: T.Text -> Maybe Event
parseEvent row = do
  [time_s, action, pkg] <- return (T.words row)
  Right (time, ":") <- return (T.decimal time_s)
  phase <- case action of
    "Configuring" -> return "B"
    "Installed" -> return "E"
    _ -> fail ""
  let pkg_name = T.dropWhileEnd (=='.') pkg
  return (Event "cabal-install" 0 0 time phase pkg_name Nothing (Args []))

parseDeps :: T.Text -> Deps
parseDeps = mapMaybe parseDep . T.lines

parseDep :: T.Text -> Maybe Dep
parseDep row = do
  [pkg0, "->", dependency0] <- return (T.words row)
  pkg <- unquote pkg0
  dependency <- unquote dependency0
  return (Dep pkg dependency)

unquote :: T.Text -> Maybe T.Text
unquote str | T.length str >= 2 && T.head str == '"' && T.last str == '"' = Just $! T.init . T.tail $ str
unquote _ = Nothing