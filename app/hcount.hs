{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List qualified as List
import Data.Ord
import Data.Bifunctor
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import NumHask.Prelude
import Options.Applicative as OA
import System.Directory.Recursive
import System.Directory
import System.FilePath
import Optics.Core
import Data.String.Interpolate
import System.Process
import FlatParse.Basic qualified as FP
import Data.ByteString qualified as BS
import Language.Haskell.Syntax.Module.Name
import GHC.Types.Name
import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Types.Name.Cache
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types

data Options = Options
  { arg :: ArgType,
    run :: RunType,
    stem :: String,
    directory :: String,
    recursive :: Bool,
    exclude :: [String],
    topN :: Int
  }
  deriving (Eq, Show, Generic)

defaultOptions :: Options
defaultOptions = Options ArgReport RunAll ".hie" "." True [] 10

options :: Parser Options
options =
  Options <$>
  parseArg <*>
  parseRun <*>
  option str (value (stem defaultOptions) <> short 's' <> long "stem" <> help "hie files directory") <*>
  option str (value (directory defaultOptions) <> long "directory" <> short 'd' <> help "base directory") <*>
  switch (long "recursize" <> short 'r' <> help "search recursively") <*>
  many (option str (short 'e' <> long "exclude" <> help "libraries to exclude")) <*>
  option auto (value (topN defaultOptions) <> short 'n' <> long "topn" <> help "report top N results")

data RunType = RunAll | RunUpper | RunLocal | RunOperators | RunLower deriving (Eq, Show, Generic)

data ArgType = ArgReport | ArgAddPcl | ArgRebuild deriving (Eq, Show, Generic)

parseRun :: Parser RunType
parseRun =
  flag' RunAll (long "all" <> help "all names")
    <|> flag' RunOperators (long "operators" <> help "operators")
    <|> flag' RunLower (long "lower" <> help "lower-case functions")
    <|> flag' RunUpper (long "upper" <> help "upper-case constructors")
    <|> flag' RunLocal (long "local" <> help "local variable names")
    <|> pure RunAll

parseArg :: Parser ArgType
parseArg =
  f <$> argument str (metavar "ARG")
  where
    f :: String -> ArgType
    f "report" = ArgReport
    f "rebuild" = ArgRebuild
    f "addpcl" = ArgAddPcl
    f _ = ArgReport

infoOptions:: ParserInfo Options
infoOptions =
  info
    (options <**> helper)
    (fullDesc <> progDesc "hcount" <> header "count Haskell artifacts")

main :: IO ()
main = do
  o <- execParser infoOptions
  case view #arg o of
    ArgReport -> report o
    ArgRebuild -> do
      n <- rebuild o
      putStrLn $ "rebuilt " <> show n <> " packages"
    ArgAddPcl -> do
      n <- addpcl o
      putStrLn $ "added " <> show n <> " pcl's"

-- | report to stdout
-- >>> report defaultOptions
-- Number of repos: 1
-- Number of files: 1
-- CCon  ~                 71
-- CCon  Monad             58
-- CCon  Functor           56
-- CCon  Category          54
-- CCon  Semigroup         50
-- CCon  Eq                49
-- CVars x                 37
-- CCon  Ord               34
-- CCon  FromInteger       29
-- CCon  Show              28
report :: Options -> IO ()
report o = do
  fs <- getFiles o
  let ns = fs & getNodes & fmap astFlatten & mconcat & getNames & getSimplifiedNames
  let n = view #topN o
  putStrLn $ ("Number of repos: ") <> show (length fs)
  putStrLn $ ("Number of files: ") <> show (length $ mconcat fs)
  case view #run o of
    RunAll -> reportTopX n (const True) ns
    RunOperators -> reportTopX n (not . FP.isLatinLetter . head . snd) ns
    RunLower -> reportTopX n (\x -> ((CExt==) . fst) x && (Char.isLower . head . snd) x) ns
    RunUpper -> reportTopX n (Char.isUpper . head . snd) ns
    RunLocal -> reportTopX n (\x -> ((CVars==) . fst) x && (Char.isLower . head . snd) x) ns

reportTopX :: Int -> ((NameCats, String) -> Bool) -> [(NameCats, String)] -> IO ()
reportTopX n p xs = mapM_ putStrLn $ formatCount 20 . first (\(c,name) -> padr 6 (show c) <> name) <$>  (Main.top n $ (filter p xs))

-- | Get the hie files, possibly recursively.
getFiles :: Options -> IO [[HieFile]]
getFiles o = do
  dirs <- case view #recursive o of
    True -> getSubdirsRecursive (view #directory o)
    False -> getDirectoryContents (view #directory o) >>= filterM doesDirectoryExist
  let hs = filter (List.isSuffixOf (stem o)) dirs
  mapM readHieFiles hs


-- | Extract the Hie AST
getNodes :: [[HieFile]] -> [HieAST TypeIndex]
getNodes fss =
  mconcat fss &
  fmap (hie_asts >>> getAsts >>> Map.toList) &
  mconcat &
  fmap snd

-- | Extract NodeInfo, throwing structural elements (and everything else) away
astFlatten :: HieAST a -> [NodeInfo a]
astFlatten h = (Map.elems $ getSourcedNodeInfo $ sourcedNodeInfo h) <> mconcat (fmap astFlatten (nodeChildren h))

-- | Extract the 'Name's from the 'NodeInfo'
getNames :: [NodeInfo a] -> [Name]
getNames ns = [x | (Right x) <- idents]
  where
    idents = ns & fmap nodeIdentifiers & fmap Map.toList & mconcat & fmap fst

-- | Simplify 'Name' to the hcount categories and a string.
getSimplifiedNames :: [Name] -> [(NameCats, String)]
getSimplifiedNames ids = allNames
  where
    xs = ids & fmap toNameX
    lxs = view #name <$> filter (\x -> module' x == "") xs
    exs = filter (\x -> module' x /= "") xs
    allNames =
      (second FP.utf8ToStr <$> rp deconstructLocalName <$> lxs) <>
      ((CExt,) . view #name <$> exs)

-- | A simplified categorization of name types
data NameCats
  = -- | "$_in$$c" prefix
    COps
  | -- | "$_in$$d" prefix
    CCon
  | -- | "$_in$" prefix
    CVars
  | COther
  | CError
  | -- | external name
    CExt
  deriving (Generic, Eq, Ord, Show)

deconstructLocalName :: FP.Parser () (NameCats, BS.ByteString)
deconstructLocalName =
  $(FP.switch [| case _ of
    "$_in$$d" -> (CCon,) <$> FP.takeRest
    "$_in$$c" -> (COps,) <$> FP.takeRest
    "$_in$$t" -> (COther,) <$> FP.takeRest
    "$_in$$maxtag_" -> (COther,) <$> FP.takeRest
    "$_in$$tag2con_" -> (COther,) <$> FP.takeRest
    "$_in$$" -> (CError,) <$> FP.takeRest
    "$_in$" -> (CVars,) <$> FP.takeRest
    _ -> (CError,) <$> FP.takeRest
    |])

package' :: FP.Parser () String
package' = (reverse . (\x -> bool id (drop 1) (head x == '-') x) . reverse) <$> FP.some (FP.satisfy (not . FP.isDigit))

-- | The main data of interest in a 'Name'
data NameX = NameX { name :: String, module' :: String, package :: String } deriving (Generic, Eq, Show, Ord)

-- | xtrct snsbl nms
toNameX :: Name -> NameX
toNameX n =
  bool
  (NameX (nameStableString n) "" "")
  (NameX (occNameString $ nameOccName n) (moduleNameString $ GHC.Unit.Types.moduleName $ nameModule n) (GHC.Data.FastString.unpackFS $ unitIdFS $ GHC.Unit.Types.moduleUnitId $ nameModule n))
  (isExternalName n)

-- | count occurrences
count :: (Ord a) => [a] -> Map.Map a Int
count = Map.fromListWith (+) . fmap (,1)

-- | top n occurrences
top :: Ord k => Int -> [k] -> [(k, Int)]
top t xs = take t $ List.sortOn (Down . snd) . Map.toList $ count xs

-- | run an FP.Parser; leftovers is an error
rp :: FP.Parser () a -> String -> a
rp p s = case FP.runParserUtf8 p s of
  FP.OK r "" -> r
  _ -> error "parser error"

-- | run an FP.Parser; leftovers are thrown away
rp' :: FP.Parser () a -> String -> a
rp' p s = case FP.runParserUtf8 p s of
  FP.OK r _ -> r
  _ -> error "parser error"


padl :: Int -> String -> String
padl n t = replicate (n - length t) ' ' <> t

padr :: Int -> String -> String
padr n t = t <> replicate (n - length t) ' '

formatCount :: Int -> (String, Int) -> String
formatCount n (s, x) = padr n s <> padl 6 (show x)

-- | get contents of all @.hie@ files recursively in the given directory
readHieFiles :: FilePath -> IO [HieFile]
readHieFiles hieDir = do
    nameCache <- initNameCache 'h' []
    hieContent <- getDirRecursive hieDir
    let isHieFile f = (&&) (takeExtension f == ".hie") <$> doesFileExist f
    hiePaths <- filterM isHieFile hieContent

    forM hiePaths $ \hiePath -> do
        hieFileResult <- readHieFile nameCache hiePath
        pure $ hie_file_result hieFileResult

pcl :: String
pcl = [i|program-options
  ghc-options:
    -fwrite-ide-info
    -hiedir=.hie|]

hasCabalFile :: FilePath -> IO Bool
hasCabalFile d = d & listDirectory & fmap (filter ((==".cabal") . takeExtension) >>> null >>> not)

hasPCL :: FilePath -> IO Bool
hasPCL d = doesFileExist $ d </> "cabal.project.local"

cabalFilePath :: FilePath -> FilePath
cabalFilePath d = d </> takeBaseName d <.> "cabal"

getSubdirs :: FilePath -> IO [FilePath]
getSubdirs d = d & (listDirectory >=> filterM (doesDirectoryExist . (\x -> d </> x)))

addPCL :: FilePath -> IO Bool
addPCL d = do
  b <- (\p c -> not p && c) <$> hasPCL d <*> hasCabalFile d
  when b (writeFile (d </> "cabal.project.local") pcl)
  return b

subdirAction :: (FilePath -> IO a) -> FilePath -> IO [a]
subdirAction a d = do
  xs <- getSubdirs d
  mapM a ((d </>) <$> xs)

dirAction :: (FilePath -> IO a) -> FilePath -> IO [a]
dirAction a d = fmap (:[]) (a d)

-- | add PCLs - single level only
addPCLs :: FilePath -> IO Int
addPCLs h = do
  xs <- getSubdirs h
  foldr ((+) . bool 0 1) 0 <$> mapM addPCL xs

recursiveAction :: (FilePath -> IO Bool) -> Options -> IO Int
recursiveAction act o =
  sum . fmap (bool 0 1) <$> (bool dirAction subdirAction (view #recursive o)) act (view #directory o)

addpcl :: Options -> IO Int
addpcl o = recursiveAction addPCL o

rebuild :: Options -> IO Int
rebuild o = recursiveAction doRebuild o

doRebuild :: FilePath -> IO Bool
doRebuild fp = do
  b <- hasCabalFile fp
  when b $ do
    putStrLn ("rebuilding " <> fp)
    callCommand "cabal clean && cabal build all"
  pure b

