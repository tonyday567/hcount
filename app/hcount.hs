{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Ord
import Circuit.Parser qualified as P
import GHC.Generics
import GHC.Data.FastString
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Unit.Types
import Language.Haskell.Syntax.Module.Name
import Optics.Core
import Options.Applicative as OA
import System.Directory
import System.Directory.Recursive
import System.FilePath
import System.Process

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
  Options
    <$> parseCommand
    <*> parseRun
    <*> option str (value (stem defaultOptions) <> short 's' <> long "stem" <> help "hie files directory")
    <*> option str (value (directory defaultOptions) <> long "directory" <> short 'd' <> help "base directory")
    <*> switch (long "recursize" <> short 'r' <> help "search recursively")
    <*> many (option str (short 'e' <> long "exclude" <> help "libraries to exclude"))
    <*> option auto (value (topN defaultOptions) <> short 'n' <> long "topn" <> help "report top N results")

data RunType = RunAll | RunUpper | RunLocal | RunOperators | RunLower deriving (Eq, Show, Generic)

data ArgType = ArgReport | ArgAddCpl | ArgBuild | ArgRebuild deriving (Eq, Show, Generic)

parseRun :: Parser RunType
parseRun =
  flag' RunAll (long "all" <> help "all names")
    <|> flag' RunOperators (long "operators" <> help "operators")
    <|> flag' RunLower (long "lower" <> help "lower-case functions")
    <|> flag' RunUpper (long "upper" <> help "upper-case constructors")
    <|> flag' RunLocal (long "local" <> help "local variable names")
    <|> pure RunAll

parseCommand :: Parser ArgType
parseCommand =
  subparser $
    command "report" (info (pure ArgReport) (progDesc "report counts."))
      <> command "build" (info (pure ArgBuild) (progDesc "build projects"))
      <> command "rebuild" (info (pure ArgRebuild) (progDesc "rebuild projects."))
      <> command "addcpl" (info (pure ArgAddCpl) (progDesc "Add a CPL file."))

infoOptions :: ParserInfo Options
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
    ArgBuild -> do
      n <- build o
      putStrLn $ "built " <> show n <> " packages"
    ArgAddCpl -> do
      n <- addcpl o
      putStrLn $ "added " <> show n <> " cpl's"

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
  putStrLn $ "Number of repos: " <> show (length fs)
  putStrLn $ "Number of files: " <> show (length $ mconcat fs)
  case view #run o of
    RunAll -> reportTopX n (const True) printName ns
    RunOperators -> reportTopX n (not . Char.isLetter . head . snd) printName ns
    RunLower -> reportTopX n (\x -> ((CExt ==) . fst) x && (Char.isLower . head . snd) x) printName ns
    RunUpper -> reportTopX n (Char.isUpper . head . snd) printName ns
    RunLocal -> reportTopX n (\x -> ((CVars ==) . fst) x && (Char.isLower . head . snd) x) printName ns

reportTopX :: (Ord a) => Int -> (a -> Bool) -> (a -> String) -> [a] -> IO ()
reportTopX n p pr xs = mapM_ putStrLn $ formatCount 20 . first pr <$> Main.top n (filter p xs)

printName :: (NameCats, String) -> String
printName (c, name) = padr 6 (show c) <> name

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
  mconcat fss
    & fmap (hie_asts >>> getAsts >>> Map.toList)
    & mconcat
    & fmap snd

-- | Extract NodeInfo, throwing structural elements (and everything else) away
astFlatten :: HieAST a -> [NodeInfo a]
astFlatten h = Map.elems (getSourcedNodeInfo $ sourcedNodeInfo h) <> mconcat (fmap astFlatten (nodeChildren h))

-- | Extract the 'Name's from the 'NodeInfo'
getNames :: [NodeInfo a] -> [Name]
getNames ns = [x | (Right x) <- idents]
  where
    idents = fmap (Map.toList . nodeIdentifiers) ns & mconcat & fmap fst

-- | Simplify 'Name' to the hcount categories and a string.
getSimplifiedNames :: [Name] -> [(NameCats, String)]
getSimplifiedNames ids = allNames
  where
    xs = ids & fmap toNameX
    lxs = view #name <$> filter (\x -> module' x == "") xs
    exs = filter (\x -> module' x /= "") xs
    allNames =
      (second B.unpack . rp deconstructLocalName <$> lxs)
        <> ((CExt,) . view #name <$> exs)

-- | A simplified categorization of name types
data NameCats
  = -- | Operators ("$_in$$c" prefix)
    COps
  | -- | Contructors ("$_in$$d" prefix)
    CCon
  | -- | Variables ("$_in$" prefix)
    CVars
  | COther
  | CError
  | -- | external project name
    CExt
  deriving (Generic, Eq, Ord, Show)

deconstructLocalName :: P.Parser Text Char (NameCats, BS.ByteString)
deconstructLocalName =
  stringBs "$_in$$d" *> ((CCon,) <$> (encodeUtf8 <$> P.takeRest))
    P.<|> stringBs "$_in$$c" *> ((COps,) <$> (encodeUtf8 <$> P.takeRest))
    P.<|> stringBs "$_in$$t" *> ((COther,) <$> (encodeUtf8 <$> P.takeRest))
    P.<|> stringBs "$_in$$maxtag_" *> ((COther,) <$> (encodeUtf8 <$> P.takeRest))
    P.<|> stringBs "$_in$$tag2con_" *> ((COther,) <$> (encodeUtf8 <$> P.takeRest))
    P.<|> stringBs "$_in$$" *> ((CError,) <$> (encodeUtf8 <$> P.takeRest))
    P.<|> stringBs "$_in$" *> ((CVars,) <$> (encodeUtf8 <$> P.takeRest))
    P.<|> ((CError,) <$> (encodeUtf8 <$> P.takeRest))

stringBs :: BS.ByteString -> P.Parser Text Char ()
stringBs bs = () <$ P.string (B.unpack bs)

-- | run a Parser; leftovers is an error
rp :: P.Parser Text Char a -> String -> a
rp p s = case P.runParser p (decodeUtf8With lenientDecode (B.pack s)) of
  P.These r rest | T.null rest -> r
  _ -> error "parser error"

-- | The main data of interest in a 'Name'
data NameX = NameX {name :: String, module' :: String, package :: String} deriving (Generic, Eq, Show, Ord)

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
top :: (Ord k) => Int -> [k] -> [(k, Int)]
top t xs = take t $ List.sortOn (Down . snd) . Map.toList $ count xs

padl :: Int -> String -> String
padl n t = replicate (n - length t) ' ' <> t

padr :: Int -> String -> String
padr n t = t <> replicate (n - length t) ' '

formatCount :: Int -> (String, Int) -> String
formatCount n (s, x) = padr n s <> padl 6 (show x)

-- | get contents of all @.hie@ files recursively in the given directory
readHieFiles :: FilePath -> IO [HieFile]
readHieFiles hieDir = do
  nameCache <- newNameCache
  hieContent <- getDirRecursive hieDir
  let isHieFile f = (&&) (takeExtension f == ".hie") <$> doesFileExist f
  hiePaths <- filterM isHieFile hieContent

  forM hiePaths $ \hiePath -> do
    hieFileResult <- readHieFile nameCache hiePath
    pure $ hie_file_result hieFileResult

cpl :: String
cpl =
  "package *\n  ghc-options: -fwrite-ide-info -hiedir=.hie\n"

hasCabalFile :: FilePath -> IO Bool
hasCabalFile d = d & listDirectory & fmap (filter ((== ".cabal") . takeExtension) >>> null >>> not)

hasCPL :: FilePath -> IO Bool
hasCPL d = doesFileExist $ d </> "cabal.project.local"

getSubdirs :: FilePath -> IO [FilePath]
getSubdirs d = d & (listDirectory >=> filterM (doesDirectoryExist . (d </>)))

addCPL :: FilePath -> IO Bool
addCPL d = do
  b <- (\p c -> not p && c) <$> hasCPL d <*> hasCabalFile d
  when b (writeFile (d </> "cabal.project.local") cpl)
  return b

subdirAction :: Options -> (FilePath -> IO a) -> IO [a]
subdirAction o act = do
  xs <- getSubdirs (view #directory o)
  let xs' = filter (not . (`elem` view #exclude o)) xs
  mapM act ((view #directory o </>) <$> xs')

dirAction :: Options -> (FilePath -> IO a) -> IO [a]
dirAction o act = fmap (: []) (act (view #directory o))

recursiveAction :: (FilePath -> IO Bool) -> Options -> IO Int
recursiveAction act o =
  sum . fmap (bool 0 1) <$> bool dirAction subdirAction (view #recursive o) o act

addcpl :: Options -> IO Int
addcpl o = recursiveAction addCPL o

rebuild :: Options -> IO Int
rebuild o = recursiveAction doRebuild o

build :: Options -> IO Int
build o = recursiveAction doBuild o

doRebuild :: FilePath -> IO Bool
doRebuild fp = do
  b <- hasCabalFile fp
  when b $ do
    putStrLn ("rebuilding " <> fp)
    withCurrentDirectory
      fp
      (callCommand "cabal clean && cabal build all --ghc-options=-fwrite-ide-info --ghc-options=-hiedir=.hie")
  pure b

doBuild :: FilePath -> IO Bool
doBuild fp = do
  b <- hasCabalFile fp
  when b $ do
    putStrLn ("building " <> fp)
    withCurrentDirectory
      fp
      (callCommand "cabal build all --ghc-options=-fwrite-ide-info --ghc-options=-hiedir=.hie")
  pure b
