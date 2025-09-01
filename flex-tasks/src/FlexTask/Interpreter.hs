{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Functions using `Interpreter` to run time compile and evaluate various aspects of a task.
The interpreted code is usually supplied by accessing data stored in `FlexInst` or `FlexConf`.
-}

module FlexTask.Interpreter
  ( checkSolution
  , genFlexInst
  , prettyError
  , validateSettings
  , validDescription
  ) where


import Control.Monad                (unless, void)
import Control.Monad.Identity       (runIdentity)
import Control.Monad.Random         (RandT, StdGen, evalRandT, mkStdGen)
import Control.OutputCapable.Blocks.Type
import Control.OutputCapable.Blocks (OutputCapable, LangM)
import Data.Digest.Pure.SHA         (sha256, showDigest)
import Data.List.Extra              (headDef, intercalate, replace)
import Data.Map                     (elems)
import Data.Maybe                   (isJust)
import Data.Text                    (Text)
import Data.Text.Lazy.Encoding      (encodeUtf8)
import Data.Text.Lazy               (pack)
import Data.Typeable                (Typeable)
import Data.Tuple.Extra             (first)
import Language.Haskell.Interpreter (
    GhcError(errMsg),
    Interpreter,
    InterpreterError(..),
    infer,
    interpret,
    loadModules,
    parens,
    setImports,
    setTopLevelModules
    )
import Language.Haskell.Interpreter.Unsafe (
    unsafeRunInterpreterWithArgs
    )
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
    )
import System.Environment          (getEnv)
import System.FilePath             ((</>), (<.>))
import Text.RawString.QQ (rQ)

import FlexTask.Types (
  CommonModules(..),
  FlexConf(..),
  FlexInst(..),
  HtmlDict,
  )
import FlexTask.Processing.Text    (removeUnicodeEscape)




type GenOutput = (String, String, IO ([Text], HtmlDict))


{- |
-}
validateSettings
  :: String   -- ^ The task identifier used for caching
  -> String   -- ^ Global module
  -> String   -- ^ Module containing configuration options
  -> [(String,String)] -- ^ Additional code modules
  -> IO (Either InterpreterError (Bool,[Output]))
validateSettings taskName globalCode settingsCode extraCode = do
    filePaths <- writeUncachedAndGetPaths taskName $
      [ ("Global", globalCode)
      , ("TaskSettings", settingsCode)
      ] ++ extraCode
    runWithPackageDB (loadModules filePaths >> validate)
  where
    validate = do
      setImports
        [ "Control.Monad.Identity"
        , "Control.OutputCapable.Blocks.Generic.Type"
        , "Control.OutputCapable.Blocks"
        , "Data.Text"
        ]
      setTopLevelModules ["TaskSettings", "Global"]
      out <- interpret "validateSettings" infer
      pure $ first (isJust @()) $ runIdentity $ getOutputSequenceWithResult out

{- |
Use a `FlexConf` to generate a `FlexInst`.
Interprets `taskDataModule` to generate the input form and task data.
Apply the given method to run the generator with a seed.
-}
genFlexInst
  :: FlexConf
  -> Int          -- ^ Generator seed
  -> IO FlexInst
genFlexInst
  FlexConf{ commonModules = commonModules@CommonModules{
    taskName,
    globalModule,
    settingsModule,
    extraModules
    },
    ..}
  seed
  = do
      filePaths <- writeUncachedAndGetPaths taskName $
        [ ("Global", globalModule)
        , ("TaskSettings", settingsModule)
        , ("TaskData", taskDataModule)
        ] ++ extraModules
      helperPath <- cacheHelper
      taskAndFormResult <- runWithPackageDB $
          loadModules (helperPath : filePaths) >> tfInter
      let gen = extract taskAndFormResult
      (taskData, checkModule, io) <- evalRandT gen $ mkStdGen seed
      form <- io
      pure $ FlexInst {
        form,
        taskData,
        checkModule,
        commonModules
      }
    where
      tfInter :: Interpreter (RandT StdGen IO GenOutput)
      tfInter = do
        setTopLevelModules ["TaskData", "Global", "TaskSettings", "Helper"]
        setImports [
            "Capabilities.Alloy.IO"
          , "Capabilities.Diagrams.IO"
          , "Capabilities.Graphviz.IO"
          , "Control.Monad.Random"
          , "Data.Generics.Text"
          , "Data.Map"
          , "Data.Text"
          , "Data.Tuple.Extra"
          ]
        interpret "third3 getFormData . first3 gshow <$> getTask " infer



makeDescription
  :: (OutputCapable m, Typeable m)
  => String
  -> String
  -> String
  -> String
  -> String
  -> [(String,String)]
  -> FilePath
  -> IO (Either InterpreterError (LangM m))
makeDescription taskName taskData global settings description extras picPath = do
    filePaths <- writeUncachedAndGetPaths taskName $
          [ ("Global", global)
          , ("TaskSettings", settings)
          , ("Description", description)
          ] ++ extras
    runWithPackageDB $ loadModules filePaths >> descInter
  where
    descInter = do
      setTopLevelModules ["Description", "Global", "TaskSettings"]
      setImports
        [ "Capabilities.Graphviz.IO"
        , "Capabilities.Cache.IO"
        , "Capabilities.Diagrams.IO"
        , "Capabilities.LatexSvg.IO"
        , "Capabilities.WriteFile.IO"
        , "Control.OutputCapable.Blocks.Generic.Type"
        , "Data.Generics.Text"
        , "Data.List.Extra"
        , "Data.Text"
        ]
      interpret ("description " ++ show picPath ++ parens (greadError taskData)) infer



{- |
Produce the task description by using task data
and two interpreted modules or restore a cached result.
Should the solution not yet exist on disc,
then it will be created by interpreting /description/ and saved in a file.
If the task description already exists on disc, it is read.
Then, if any of the image links of that description are invalid (have been deleted),
the description is interpreted again to regenerate the missing files.
-}
validDescription
  :: OutputCapable m
  => String       -- ^ The task identifier used for caching
  -> String       -- ^ Data available for making the description
  -> String       -- ^ Global module
  -> String       -- ^ Settings module
  -> String       -- ^ Module containing the /description/ function
  -> [(String,String)] -- ^ Additional code modules
  -> FilePath     -- ^ Path images will be stored in
  -> IO (LangM m) -- ^ `OutputCapable` representation of task description
validDescription taskName taskData globalModule settingsModule descModule extras picPath = do
  let (firstHalf,secondHalf) = splitAt (length extras `div` 2) $ map snd extras
  let fileName = intercalate "-" $ "DescriptionCache" : map hash
        [ descModule ++ concat firstHalf
        , taskData ++ concat secondHalf
        , globalModule ++ settingsModule
        ]
  cDir <- cacheDir taskName
  let path = cDir </> fileName
  isThere <- doesFileExist path
  if isThere
    then do
      output <- read <$> readFile path
      let fileLinks = imageLinks output
      exist <- mapM doesFileExist fileLinks
      if and exist
        then
          return $ toOutputCapable output
        else
          makeDescAndWrite (Just output) path
    else
      makeDescAndWrite Nothing path
  where
    makeDescAndWrite mOldOutput p = do
      res <- makeDescription taskName taskData globalModule settingsModule descModule extras picPath
      output <- getOutputSequence $ extract res
      unless (mOldOutput == Just output) $ writeFile p $ show output
      return $ toOutputCapable output



{- |
Run the interpreter with a custom package database.
The filepath is given externally via an environment variable /FLEX_PKGDB/.
-}
runWithPackageDB :: Interpreter a -> IO (Either InterpreterError a)
runWithPackageDB interpreter = do
  path <- getEnv "FLEX_PKGDB"
  unsafeRunInterpreterWithArgs ["-package-db " <> path] interpreter



{- |
Use task data and interpret three code modules to evaluate a submission.
The submission ist parsed by function /parseSubmission/.
The result is evaluated by functions /checkSyntax/ and /checkSemantics/.
The result is a tuple of syntax feedback and optional semantics feedback.
If the syntax check fails, then no semantics feedback is provided.
Semantics feedback is coupled with a rating given as a Rational (0 to 1).
-}
checkSolution
  :: String   -- ^ The task identifier used for caching
  -> String   -- ^ Data made available to checker functions
  -> String   -- ^ Global module
  -> String   -- ^ Module containing configuration options
  -> String   -- ^ Module containing /parseSubmission/
  -> String   -- ^ Module containing /checkSyntax/ and /checkSemantics/
  -> [(String,String)] -- ^ Additional code modules
  -> String   -- ^ Student solution
  -> FilePath -- ^ Path images will be stored in
  -> IO (Either InterpreterError ([Output], Maybe (Maybe Rational, [Output])))
checkSolution taskName taskData globalCode settingsCode parseCode checkCode extraCode submission picPath = do
    filePaths <- writeUncachedAndGetPaths taskName $
      [ ("Global", globalCode)
      , ("TaskSettings", settingsCode)
      , ("Parse", parseCode)
      , ("Check", checkCode)
      ] ++ extraCode
    helperPath <- cacheHelper
    runWithPackageDB (loadModules (helperPath : filePaths) >> runCheck) >>= sequence
  where
    runCheck = do
      setImports
        [ "Capabilities.Cache.IO"
        , "Capabilities.Diagrams.IO"
        , "Capabilities.LatexSvg.IO"
        , "Capabilities.Graphviz.IO"
        , "Capabilities.WriteFile.IO"
        , "Control.OutputCapable.Blocks.Generic.Type"
        , "Control.OutputCapable.Blocks"
        , "Data.Generics.Text"
        , "Data.List.Extra"
        , "Data.Ratio"
        , "Data.Text"
        ]
      setTopLevelModules ["Check", "Global", "Helper", "Parse"]
      interpret ("syntaxAndSemantics parseSubmission checkSyntax checkSemantics " ++ input ++ path ++ tData) infer

    tData = parens $ greadError taskData
    input = removeUnicodeEscape (show $ replace "\\\\" "\\" submission)
    path = show picPath



writeUncachedAndGetPaths :: FilePath -> [(String, String)] -> IO [FilePath]
writeUncachedAndGetPaths cachePrefix xs = do
    paths <- getCachePaths xs
    writeUncachedFiles paths
    pure $ map fst paths
  where
    getCachePaths :: [(String,String)] -> IO [(FilePath,String)]
    getCachePaths files = do
      dir <- cacheDir cachePrefix
      pure $ map (\(prefix, content) ->
                 (dir </> prefix <> "-" <> hash content <.> "hs",content)) files

    writeUncachedFiles :: [(FilePath,String)] -> IO ()
    writeUncachedFiles = void . mapM (\ (path,content) ->
      doesFileExist path >>= flip unless (writeFile path content))



extract :: Either InterpreterError c -> c
extract = either (error . prettyError) id


hash :: Show a => a -> String
hash = showDigest . sha256 . encodeUtf8 . pack . show



cacheDir :: FilePath -> IO FilePath
cacheDir prefix = do
  temporary <- getTemporaryDirectory
  let dir = temporary </> "FlexCache" </> prefix
  createDirectoryIfMissing True dir
  pure dir



imageLinks :: [Output] -> [FilePath]
imageLinks = concatMap $ foldMapOutputBy (++) (\case
  Image l       -> [l]
  Images m      -> elems m
  YesNo {}      -> []
  Paragraph {}  -> []
  Enumerated {} -> []
  Itemized {}   -> []
  Indented {}   -> []
  Folded {}     -> []
  Latex {}      -> []
  Code {}       -> []
  Translated {} -> []
  Special {}    -> []
  )


cacheHelper :: IO FilePath
cacheHelper = headDef cachingError <$>
    writeUncachedAndGetPaths "" [("Helper",helperCode)]
  where
    cachingError = error "Caching the internal Helper module failed."

    helperCode = [rQ|
module Helper (syntaxAndSemantics) where

import FlexTask.InterpreterHelper
|]



{- |
Custom display of Hint InterpreterError messages.
-}
prettyError :: InterpreterError -> String
prettyError (UnknownError s) = "Unknown error:\n" ++ s
prettyError (NotAllowed s) = "Not allowed:\n" ++ s
prettyError (GhcException s) = "GHC exception occurred:\n" ++ s
prettyError (WontCompile ghcErrors) = "Won't compile:\n" ++ unlines (map errMsg ghcErrors)


greadError :: String -> String
greadError term = "fst $ headDef (error " ++ show errorMessage ++") $ gread " ++ show term ++ " :: TaskData"
  where
    errorMessage = "Failed reading stored TaskData. Encountered this: " ++ term
