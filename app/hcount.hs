{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import HieTypes
import NumHask.Prelude
import Options.Applicative as OA
import Stan.Ghc.Compat
import Stan.Hie

main :: IO ()
main = do
  hccfg <- execParser (info (hc <**> helper) (fullDesc <> progDesc "Count haskell names using hie files"))
  report hccfg

-- | report to stdout
-- >>> report defaultHCountConfig
report :: HCountConfig -> IO ()
report cfg = do
  fs <- mapM readHieFiles (unpack <$> hcHiefiles cfg)
  putStrLn $ ("Number of repos: " :: Text) <> show (length fs)
  putStrLn $ ("Number of files: " :: Text) <> show (length $ mconcat fs)
  let ns = getNames fs
  putStrLn $ ("Total names: " :: Text) <> show (sum . fmap snd $ ns)
  putStrLn $ ("Unique names: " :: Text) <> show (length ns)

  when (hcAll cfg) $ do
    putStrLn ("top names" :: Text)
    putStrLn ("total names: " <> show (sum $ snd <$> ns) :: Text)
    putStrLn ("" :: Text)
    mapM_ putStrLn (fmap formatResults $ take (hcTopn cfg) $ first locname <$> ns)

  when (hcOperators cfg) $ do
    putStrLn ("top operators" :: Text)
    let op' = filter (oph (hcExcluded cfg) . fst) ns
    putStrLn ("total operators: " <> show (sum $ snd <$> op') :: Text)
    putStrLn ("" :: Text)
    mapM_ putStrLn (fmap formatResults $ take (hcTopn cfg) $ first locname <$> op')

  when (hcFunctions cfg) $ do
    putStrLn ("top lower case functions" :: Text)
    let lower' = filter (lowerh (hcExcluded cfg) . fst) ns
    putStrLn ("total functions: " <> show (sum $ snd <$> lower') :: Text)
    putStrLn ("" :: Text)
    mapM_ putStrLn (fmap formatResults $ take (hcTopn cfg) $ first locname <$> lower')

  when (hcConstructors cfg) $ do
    putStrLn ("top upper case constructors" :: Text)
    let upper' = filter (upperh (hcExcluded cfg) . fst) ns
    putStrLn ("total constructors: " <> show (sum $ snd <$> upper') :: Text)
    putStrLn ("" :: Text)
    mapM_ putStrLn (fmap formatResults $ take (hcTopn cfg) $ first locname <$> upper')

  when (hcLocals cfg) $ do
    putStrLn ("top local names" :: Text)
    let local' = filter (localh . fst) ns
    putStrLn ("total local names: " <> show (sum $ snd <$> local') :: Text)
    putStrLn ("" :: Text)
    mapM_ putStrLn (fmap formatResults $ take (hcTopn cfg) $ first locname <$> local')

getNames :: [[HieFile]] -> [((Loc, Text), Int)]
getNames = sortOn (Down . snd) . Map.toList . Main.count . names . fmap fst . mconcat . fmap astFlat . asts . mconcat

-- | Get the trees from each hie file, throwing away file details
asts :: [HieFile] -> [HieAST TypeIndex]
asts fs = mconcat $ Map.elems . getAsts . hie_asts <$> fs

-- | Throw away the tree structure and retain node information.
--
-- deconstruction of:
-- sum = P.foldr (+) zero
--
-- P.foldr
-- NodeInfo {nodeAnnotations = fromList [("HsVar","HsExpr"),("HsWrap","HsExpr")], nodeType = [7,24], nodeIdentifiers = fromList [(Right NameMeta {nameMetaPackage = "base", nameMetaModuleName = ModuleName {unModuleName = "Data.Foldable"}, nameMetaName = "foldr"},IdentifierDetails {identType = Just 24, identInfo = fromList [Use]})]}
--
-- (+)
--
-- NodeInfo {nodeAnnotations = fromList [("HsVar","HsExpr"),("HsWrap","HsExpr")], nodeType = [5,30], nodeIdentifiers = fromList [(Right NameMeta {nameMetaPackage = "numhask-0.7.0.1-7TAYiN9QGfDA9z2ZI270ii", nameMetaModuleName = ModuleName {unModuleName = "NumHask.Algebra.Additive"}, nameMetaName = "+"},IdentifierDetails {identType = Just 30, identInfo = fromList [Use]})]}
--
-- zero
--
-- NodeInfo {nodeAnnotations = fromList [("HsVar","HsExpr"),("HsWrap","HsExpr")], nodeType = [1,32], nodeIdentifiers = fromList [(Right NameMeta {nameMetaPackage = "numhask-0.7.0.1-7TAYiN9QGfDA9z2ZI270ii", nameMetaModuleName = ModuleName {unModuleName = "NumHask.Algebra.Additive"}, nameMetaName = "zero"},IdentifierDetails {identType = Just 32, identInfo = fromList [Use]})]}
--
-- LogField $ negate x
--
-- NodeInfo {nodeAnnotations = fromList [("HsConLikeOut","HsExpr"),("HsVar","HsExpr"),("HsWrap","HsExpr")], nodeType = [1246,1084], nodeIdentifiers = fromList [(Right NameMeta {nameMetaPackage = "numhask-0.7.0.1-7TAYiN9QGfDA9z2ZI270ii", nameMetaModuleName = ModuleName {unModuleName = "NumHask.Data.LogField"}, nameMetaName = "LogField"},IdentifierDetails {identType = Nothing, identInfo = fromList [Use]})]}
--
-- NodeInfo {nodeAnnotations = fromList [("HsVar","HsExpr"),("HsWrap","HsExpr")], nodeType = [1247,1256], nodeIdentifiers = fromList [(Right NameMeta {nameMetaPackage = "base", nameMetaModuleName = ModuleName {unModuleName = "GHC.Base"}, nameMetaName = "$"},IdentifierDetails {identType = Just 1256, identInfo = fromList [Use]})]}
--
-- NodeInfo {nodeAnnotations = fromList [("HsVar","HsExpr")], nodeType = [1220], nodeIdentifiers = fromList [(Right $_in$x,IdentifierDetails {identType = Just 1220, identInfo = fromList [Use]})]}
--
-- lf = fs !! 4
-- lf207s = filter (\x -> 207 == (srcLocLine . realSrcSpanStart . snd $ x)) $ (mconcat $ astFlat <$> asts [lf])
astFlat :: HieAST a -> [(NodeInfo a, Span)]
astFlat h = [(nodeInfo h, nodeSpan h)] <> mconcat (fmap astFlat (nodeChildren h))

data Loc = Local | Main | Base | Unknown | Package Text deriving (Show, Eq, Ord)

-- | extract simplified names of variables from the NodeInfo
names :: [NodeInfo a] -> [(Loc, Text)]
names nodes =
  [ simplifyLocName $ pack $ nameStableString x
    | (Right x) <-
        mconcat $
          Map.keys . nodeIdentifiers
            <$> filter (Set.member ("HsVar", "HsExpr") . nodeAnnotations) nodes
  ]

-- | count occurrences
count :: (Ord a) => [a] -> Map.Map a Int
count = Map.fromListWith (+) . fmap (,1)

-- | simplified location name
simplifyLocName :: Text -> (Loc, Text)
simplifyLocName t = either (const (Unknown, t)) id $ A.parseOnly lp t
  where
    lp = pLocal' <|> pLocal <|> pBase' <|> pBase <|> pPackage' <|> pPackage <|> pMain

pLocal :: A.Parser (Loc, Text)
pLocal = do
  _ <- A.string "$_in$"
  n <- A.takeText
  pure (Local, n)

pLocal' :: A.Parser (Loc, Text)
pLocal' = do
  _ <- A.string "$_in$"
  _ <- A.string "$d" <|> A.string "$cp1" <|> A.string "$cp2" <|> A.string "$cp3" <|> A.string "$cp4" <|> A.string "$c" <|> A.string "$C:"
  n <- A.takeText
  pure (Local, n)

pBase' :: A.Parser (Loc, Text)
pBase' = do
  _ <- A.string "$base$"
  _ <- A.skipWhile (/= '$')
  _ <- A.char '$'
  _ <- A.string "C:"
  n <- A.takeText
  pure (Base, n)

pBase :: A.Parser (Loc, Text)
pBase = do
  _ <- A.string "$base$"
  _ <- A.skipWhile (/= '$')
  _ <- A.char '$'
  n <- A.takeText
  pure (Base, n)

-- "numhask-space-0.7.0.0-LPYAlOQ382DJicX4NZ3Jtj"
--
pPackageName :: A.Parser Text
pPackageName = do
  ps <- A.sepBy1 (A.takeWhile (`notElem` ['-', '$'])) (A.char '-')
  case reverse ps of
    (_ : _ : ps') -> pure $ Text.intercalate "-" (reverse ps')
    ps' -> pure $ Text.intercalate "-" (reverse ps')

pPackage' :: A.Parser (Loc, Text)
pPackage' = do
  _ <- A.char '$'
  p <- pPackageName
  _ <- A.char '$'
  _ <- A.skipWhile (/= '$')
  _ <- A.char '$'
  _ <- A.string "C:"
  n <- A.takeText
  pure (Package p, n)

pPackage :: A.Parser (Loc, Text)
pPackage = do
  _ <- A.char '$'
  p <- pPackageName
  _ <- A.char '$'
  _ <- A.skipWhile (/= '$')
  _ <- A.char '$'
  n <- A.takeText
  pure (Package p, n)

pMain :: A.Parser (Loc, Text)
pMain = do
  _ <- A.string "$main$"
  _ <- A.skipWhile (/= '$')
  _ <- A.char '$'
  n <- A.takeText
  pure (Main, n)

lowerh :: [Text] -> (Loc, Text) -> Bool
lowerh _ (Local, _) = False
lowerh _ (Main, _) = False
lowerh _ (Base, n) = Char.isLower (Text.head n)
lowerh ex (Package p, n) = Char.isLower (Text.head n) && notElem p ex
lowerh _ (Unknown, _) = False

upperh :: [Text] -> (Loc, Text) -> Bool
upperh _ (Local, _) = False
upperh _ (Main, _) = False
upperh _ (Base, n) = Char.isUpper (Text.head n)
upperh ex (Package p, n) = Char.isUpper (Text.head n) && notElem p ex
upperh _ (Unknown, _) = False

oph :: [Text] -> (Loc, Text) -> Bool
oph _ (Local, _) = False
oph _ (Main, _) = False
oph _ (Base, n) = not $ Char.isLetter (Text.head n)
oph ex (Package p, n) = not (Char.isLetter (Text.head n)) && notElem p ex
oph _ (Unknown, _) = False

locname :: (Loc, Text) -> (Text, Text)
locname (Local, n) = ("local", n)
locname (Main, n) = ("main", n)
locname (Base, n) = ("base", n)
locname (Package p, n) = (p, n)
locname (Unknown, n) = ("unknown", n)

localh :: (Loc, Text) -> Bool
localh (Local, _) = True
localh (Main, _) = True
localh (Base, _) = False
localh (Package _, _) = False
localh (Unknown, _) = True

data HCountConfig = HCountConfig {hcHiefiles :: [Text], hcExcluded :: [Text], hcTopn :: Int, hcAll :: Bool, hcOperators :: Bool, hcFunctions :: Bool, hcConstructors :: Bool, hcLocals :: Bool} deriving (Show)

defaultHCountConfig :: HCountConfig
defaultHCountConfig = HCountConfig [".hie"] [] 10 True True True True True

hc :: Parser HCountConfig
hc =
  HCountConfig
    <$> fmap
      (\x -> bool x [".hie"] (null x))
      ( many $
          OA.option
            str
            ( metavar "DIR"
                <> long "hie-directory"
                <> short 'd'
                <> help "location of hie directory (default \".hie\")"
            )
      )
    <*> many
      ( OA.option
          str
          ( metavar "TEXT"
              <> long "excluded-lib"
              <> short 'e'
              <> help "Libraries to exclude from analysis"
          )
      )
    <*> OA.option
      auto
      ( long "topn"
          <> short 't'
          <> help "How many names to print"
          <> showDefault
          <> value 10
          <> metavar "INT"
      )
    <*> flag
      True
      False
      ( long "no-all-names"
          <> short 'a'
          <> help "no report on all names"
          <> showDefault
      )
    <*> flag
      True
      False
      ( long "no-operators"
          <> short 'o'
          <> help "no report on operators"
          <> showDefault
      )
    <*> flag
      True
      False
      ( long "no-functions"
          <> short 'f'
          <> help "no report on external functions used"
          <> showDefault
      )
    <*> flag
      True
      False
      ( long "no-constructors"
          <> short 'c'
          <> help "no report on constructors used"
          <> showDefault
      )
    <*> flag
      True
      False
      ( long "no-locals"
          <> short 'l'
          <> help "no report on local names"
          <> showDefault
      )

padl :: Int -> Text -> Text
padl n t = Text.replicate (n - Text.length t) " " <> t

padr :: Int -> Text -> Text
padr n t = t <> Text.replicate (n - Text.length t) " "

formatResults :: ((Text, Text), Int) -> Text
formatResults ((l, n), x) = padr 10 l <> padr 10 n <> padl 6 (show x)

-- | current personal run
--
-- > myhc = HCountConfig ((\fp -> "../" <> fp <> "/.hie") <$> [ "box","box-csv","box-socket","chart-svg","chart-svg-graphviz","ellipse","ephemeral","formatn","hcount","iqfeed","mealy","numhask","numhask-array","numhask-backprop","numhask-bench","numhask-space","perf","readme-lhs","web-rep"]) ["lucid", "web-rep", "backprop"] 100 True True True True True
-- > report myhc
-- Number of repos: 19
-- Number of files: 102
-- Total names: 31919
-- Unique names: 3103
-- top names
-- total names: 31919
--
-- base      $            828
-- local     x            696
-- base      fromLabel    613
-- base      .            536
-- local     a            510
-- numhask   *            510
-- base      &            499
-- lens      .~           483
-- base      <$>          479
-- base      <>           422
-- local     IsLabel      418
-- lens      ^.           411
-- local     s            400
-- numhask   +            378
-- local     c            299
-- local     n            284
-- local     y            263
-- numhask   -            254
-- numhask   /            252
-- local     showList     250
-- local     showsPrec    249
-- local     show         242
-- base      Show         241
-- local     /=           236
-- local     ==           234
--          Eq           234
-- local     f            220
-- base      Just         218
-- base      Generic      216
-- numhask-spacePoint        205
-- base      pure         200
-- local     r            182
-- local     xs           180
-- numhask   zero         176
-- base      mempty       173
-- local     b            172
--          ==           172
-- numhask   one          168
-- local     p            167
-- local     t            164
-- base      Nothing      161
--          []           155
-- local     d            151
-- protolude show         143
-- local     l            141
-- numhask   fromIntegral   140
-- chart-svg Chart        131
-- local     e            127
-- local     i            110
-- local     from         108
-- local     to           108
-- protolude bool         105
-- base      fmap          97
-- local     cs            96
-- base      maybe         90
-- local     o             87
-- local     g             86
-- local     m             84
-- local     w             83
-- local     Epsilon       80
-- local     x'            80
-- numhask-spacePointXY       78
-- local     go            76
--          :             76
-- base      id            75
-- base      <*>           74
-- lucid     with          74
-- base      snd           70
-- local     cfg           67
-- base      otherwise     67
-- local     v             66
-- local     z             66
-- lucid     term          66
-- base      fst           65
-- base      zipWith       65
-- chart-svg-graphvizDependency    64
-- web-rep   class__       64
-- local     step          61
-- local     ts            60
--          pack          57
-- base      !!            54
-- numhask   negate        54
-- local     -             53
-- local     Norm          51
-- local     a'            51
-- local     Distributive    50
--          True          50
-- lens      %~            50
-- local     h             48
-- local     y'            48
-- base      mconcat       47
-- local     Signed        46
-- base      reverse       45
-- local     fromIntegral    44
--          False         44
-- box       commit        44
-- chart-svg LineA         44
-- numhask   FromIntegral    44
-- lucid     toHtml        43
-- numhask   pi            43
-- top operators
-- total operators: 6330
--
-- base      $            828
-- base      .            536
-- numhask   *            510
-- base      &            499
-- lens      .~           483
-- base      <$>          479
-- base      <>           422
-- lens      ^.           411
-- numhask   +            378
-- numhask   -            254
-- numhask   /            252
--          ==           172
--          []           155
--          :             76
-- base      <*>           74
-- base      !!            54
-- lens      %~            50
--          &&            42
--          ()            39
--          <             31
-- bifunctors<<*>>         29
-- base      <|>           28
--          >             28
-- numhask   .*            24
-- base      +             22
--          <=            21
-- numhask   ^             21
--          >=            20
-- numhask   :%            20
-- lens      ?~            19
--          ||            18
-- numhask   :+            17
-- base      *             16
-- base      ++            16
--          (,)           15
-- lens      _1            15
-- numhask   **            15
-- base      *>            14
-- base      -             14
-- box       <$.>          13
--          /=            12
-- numhask   /.            11
-- base      :|             9
-- numhask   /\             9
-- numhask   \/             9
-- numhask-space_x             9
-- numhask-space_y             9
-- base      >>             8
-- lens      _Just          8
-- numhask-space...            8
-- numhask   ^^             7
-- base      <*             6
-- lens      .=             6
-- numhask-space>.<            6
-- base      <$             5
-- chart-svg $sel:rstyle:AxisBar     5
-- microlens .~             5
-- numhask-space|.|            5
-- microlens ^.             4
-- numhask-space|.             4
--          !              3
-- chart-svg $sel:positions:TickComponents     3
-- chart-svg $sel:xys:Chart     3
-- lens      _2             3
-- base      $!             2
-- base      **             2
-- base      /              2
--          |>             2
-- chart-svg $sel:color':Colour'     2
-- chart-svg $sel:unhud:Hud     2
-- clay      ?              2
-- lens      %=             2
-- numhask   ⊕              2
-- vector    !              2
-- base      $>             1
-- base      %              1
-- base      =<<            1
-- base      >=>            1
-- base      >>=            1
--          (,,)           1
--          <|             1
-- box       <*.>           1
-- chart-svg $sel:annotation:Chart     1
-- chart-svg $sel:labels:TickComponents     1
-- hmatrix   <>             1
-- hmatrix   ><             1
-- numhask   $c:+           1
-- numhask   $cLogField     1
-- numhask   $tComplex      1
-- numhask   $tLogField     1
-- numhask   *.             1
-- numhask-space+/-            1
-- numhask-space<.>            1
-- numhask-space|<|            1
-- numhask-space|>|            1
-- semigroupoids<.>            1
-- unordered-containers!              1
-- top lower case functions
-- total functions: 7250
--
-- base      fromLabel    613
-- base      pure         200
-- numhask   zero         176
-- base      mempty       173
-- numhask   one          168
-- protolude show         143
-- numhask   fromIntegral   140
-- protolude bool         105
-- base      fmap          97
-- base      maybe         90
-- base      id            75
-- base      snd           70
-- base      otherwise     67
-- base      fst           65
-- base      zipWith       65
--          pack          57
-- numhask   negate        54
-- base      mconcat       47
-- base      reverse       45
-- box       commit        44
-- numhask   pi            43
-- chart-svg defaultLineStyle    39
-- chart-svg palette1      36
-- numhask   sin           36
-- numhask   cos           33
-- base      const         32
--          unpack        32
-- box       emit          32
-- chart-svg writeChartSvg    31
-- base      liftIO        30
-- numhask-spacelower         30
-- base      fromIntegral    29
-- base      length        29
-- base      second        29
-- base      fromMaybe     28
-- base      take          28
-- base      abs           27
-- base      zip           27
-- chart-svg defaultHudOptions    27
-- chart-svg ellipse       27
--          max           26
--          min           26
-- chart-svg defaultTextStyle    26
-- numhask   floor         26
-- numhask-spaceupper         26
-- base      return        25
-- base      fromInteger    24
-- box       glue          24
-- chart-svg fromV2        24
-- numhask-arrayshape         24
-- attoparsectakeText      23
-- chart-svg defaultGlyphStyle    23
-- foldl     fold          23
--          lift          22
-- chart-svg fromRGB       22
--          addDays       21
-- base      bimap         20
-- chart-svg defaultRectStyle    20
-- attoparsecchar          19
-- numhask   exp           19
-- numhask   sum           19
-- base      drop          18
-- base      fromString    18
-- base      show          18
-- adjunctionsindex         18
-- numhask   abs           18
-- base      foldl'        17
-- adjunctionstabulate      17
-- lens      use           17
-- numhask   sqrt          17
-- numhask   two           17
-- numhask-arrayindex         17
-- base      either        16
-- box       mapE          16
-- box       toStdout      16
-- box-csv   int'          16
-- base      flip          15
-- base      natVal        15
-- base      throw         15
-- attoparsecstring        15
-- chart-svg defaultSvgOptions    15
-- mmorph    hoist         15
-- numhask   fromBaseRational    15
-- numhask-arraytabulate      15
-- numhask-spacefoldRect      15
-- profunctorslmap          15
-- base      negate        14
-- base      signum        14
-- base      toRational    14
-- chart-svg defaultTitle    14
-- chart-svg opac          14
-- numhask-spaceaspect        14
-- numhask-spacefromNominalDiffTime    14
-- numhask-spacespace1        14
-- numhask-spacetoNominalDiffTime    14
-- numhask-spacetoPoint       14
-- numhask-spacetoRect        14
--          addUTCTime    13
-- box-csv   double'       13
-- chart-svg setOpac       13
-- top upper case constructors
-- total constructors: 4109
--
-- base      Show         241
--          Eq           234
-- base      Just         218
-- base      Generic      216
-- numhask-spacePoint        205
-- base      Nothing      161
-- chart-svg Chart        131
-- numhask-spacePointXY       78
-- chart-svg-graphvizDependency    64
--          True          50
--          False         44
-- chart-svg LineA         44
-- numhask   FromIntegral    44
-- numhask-spaceRect          42
-- chart-svg Colour        40
-- numhask   ToIntegral    37
-- chart-svg RectA         35
-- numhask-spaceRange         34
-- base      Functor       31
-- box       Box           30
-- numhask   Additive      29
-- numhask   Multiplicative    29
-- base      Left          28
-- chart-svg TextA         28
-- numhask   Subtractive    28
-- numhask   JoinSemiLattice    27
-- numhask   MeetSemiLattice    27
-- numhask-spaceRectXY        27
-- box       Emitter       26
-- base      Right         25
-- chart-svg GlyphA        25
-- numhask   Distributive    25
-- numhask   Signed        25
-- numhask-spaceP             25
--          Ord           23
-- foldl     Fold          22
-- numhask   Epsilon       22
-- base      Semigroup     21
-- box       Committer     21
-- box       Cont          21
-- base      Proxy         20
-- reanimate-svgNum           20
-- language-javascriptJSSemi        19
-- numhask   BoundedJoinSemiLattice    19
-- numhask   InvolutiveRing    19
-- numhask   ToRatio       19
-- readme-lhsBlock         19
-- chart-svg PlaceBottom    18
-- language-javascriptJSAstProgram    18
-- language-javascriptJSExpressionStatement    18
-- numhask   BoundedMeetSemiLattice    18
-- numhask   Integral      18
-- base      Foldable      17
-- base      Traversable    17
-- numhask   Norm          17
-- base      Last          16
-- base      Monoid        16
-- mealy     M             16
-- numhask   LogField      16
-- numhask-spaceR             16
--          UTCTime       15
-- base      Applicative    14
-- base      Read          14
-- chart-svg LineI         14
-- numhask   Divisive      14
-- numhask   FromInteger    14
-- numhask   NumHaskException    14
-- numhask-arrayArray         14
-- chart-svg AnchorStart    13
-- chart-svg Hud           12
-- chart-svg LineCapRound    12
-- chart-svg-graphvizRing          12
-- language-javascriptJSNoAnnot     12
-- numhask   Field         12
-- readme-lhsCode          12
-- chart-svg FixedAspect    11
-- chart-svg PathA         11
-- chart-svg PlaceLeft     11
-- chart-svg PlaceRight    11
-- chart-svg-graphvizField         11
-- chart-svg-graphvizNumHaskCluster    11
-- chart-svg AnchorEnd     10
-- chart-svg PlaceTop      10
-- numhask   ExpField      10
-- numhask   UpperBoundedField    10
-- numhask-spaceMidPos        10
-- chart-svg ArcInfo        9
-- chart-svg ArcPosition     9
-- chart-svg-graphvizAdditive       9
-- chart-svg-graphvizAssociative     9
-- chart-svg-graphvizGroupCluster     9
-- chart-svg-graphvizMultiplicative     9
-- chart-svg-graphvizUnital         9
-- numhask   LowerBoundedField     9
-- numhask   QuotientField     9
-- numhask-spaceOuterPos       9
-- chart-svg BlankA         8
-- chart-svg ChartSvg       8
-- chart-svg RectStyle      8
-- chart-svg StartI         8
-- top local names
-- total local names: 13262
--
-- local     x            696
-- local     a            510
-- local     IsLabel      418
-- local     s            400
-- local     c            299
-- local     n            284
-- local     y            263
-- local     showList     250
-- local     showsPrec    249
-- local     show         242
-- local     /=           236
-- local     ==           234
-- local     f            220
-- local     r            182
-- local     xs           180
-- local     b            172
-- local     p            167
-- local     t            164
-- local     d            151
-- local     l            141
-- local     e            127
-- local     i            110
-- local     from         108
-- local     to           108
-- local     cs            96
-- local     o             87
-- local     g             86
-- local     m             84
-- local     w             83
-- local     Epsilon       80
-- local     x'            80
-- local     go            76
-- local     cfg           67
-- local     v             66
-- local     z             66
-- local     step          61
-- local     ts            60
-- local     -             53
-- local     Norm          51
-- local     a'            51
-- local     Distributive    50
-- local     h             48
-- local     y'            48
-- local     Signed        46
-- local     fromIntegral    44
-- local     sconcat       42
-- local     stimes        42
-- local     rx            41
-- local     pl            40
-- local     aboutEqual    39
-- local     phi           39
-- local     <$            38
-- local     nearZero      37
-- local     ps            37
-- local     toIntegral    37
-- local     zero          37
-- local     c'            36
-- local     adj           34
-- local     ds            34
-- local     epsilon       34
-- local     one           33
-- local     u             33
-- local     mconcat       32
-- local     div           31
-- local     fmap          31
-- local     mod           31
-- local     quot          31
-- local     rem           31
-- local     start         31
-- local     Field         30
-- local     ry            30
-- local     so            30
-- local     *             29
-- local     +             29
-- local     x2            29
-- local     xs'           29
-- local     negate        28
-- local     p1            28
-- local     /\            27
-- local     QuotientField    27
-- local     Subtractive    27
-- local     \/            27
-- local     q             27
-- local     ann           26
-- local     fa            26
-- local     JoinSemiLattice    25
-- local     MeetSemiLattice    25
-- local     abs           25
-- local     conn          25
-- local     end           25
-- local     sign          25
-- local     /             24
-- local     >             24
-- local     >=            24
-- local     compare       24
-- local     f'            24
-- local     max           24
-- local     min           24
-- local     pc            24
-- local     <             23
