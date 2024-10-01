{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Functions using `Interpreter` to run time compile and evaluate various aspects of a task.
The interpreted code is usually supplied by accessing data stored in `FlexInst` or `FlexConf`.
-}

module FlexTask.Interpreter
  ( checkSolution
  , genFlexInst
  , runWithPackageDB
  , validDescription
  ) where


import Control.Monad                (unless, void)
import Control.OutputCapable.Blocks.Type
import Control.OutputCapable.Blocks (OutputCapable, LangM)
import Data.Digest.Pure.SHA         (sha256, showDigest)
import Data.List.Extra              (replace)
import Data.Map                     (elems)
import Data.Text.Lazy.Encoding      (encodeUtf8)
import Data.Text.Lazy               (pack)
import Data.Typeable                (Typeable)
import Language.Haskell.Interpreter (
    Interpreter,
    InterpreterError,
    infer,
    interpret,
    loadModules,
    parens,
    setImportsQ,
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
import Test.QuickCheck.Gen         (Gen)

import FlexTask.Types              (CommonModules(..), FlexConf(..), FlexInst(..))
import FlexTask.Processing.Text    (removeUnicodeEscape)




type GenOutput = (String, String, IO ([String],String))


{- |
Use a `FlexConf` to generate a `FlexInst`.
Interprets `taskDataModule` to generate the input form and task data.
Apply the given method to run the generator with a seed.
-}
genFlexInst
  :: FlexConf
  -> (Gen GenOutput -> a -> GenOutput) -- ^ Method of running the random generator
  -> a                                 -- ^ Generator seed
  -> IO FlexInst
genFlexInst
  FlexConf{commonModules = commonModules@CommonModules{globalModule}, ..}
  genMethod
  seed
  = do
      filePaths <- writeUncachedAndGetPaths
        [ ("Global", globalModule)
        , ("TaskData", taskDataModule)
        ]
      taskAndFormResult <- runWithPackageDB $
                             loadModules filePaths >> tfInter
      let gen = extract taskAndFormResult
      let (taskData, checkModule, io) = genMethod gen seed
      form <- io
      pure $ FlexInst {
        form,
        taskData,
        checkModule,
        commonModules
      }
    where
      tfInter :: Interpreter (Gen GenOutput)
      tfInter = setTopLevelModules ["TaskData"] >>
                  interpret "getTask " infer



makeDescription
  :: (OutputCapable m, Typeable m)
  => String
  -> String
  -> String
  -> FilePath
  -> IO (Either InterpreterError (LangM m))
makeDescription taskData global description picPath = do
    filePaths <- writeUncachedAndGetPaths
          [ ("Global", global)
          , ("Description", description)
          ]
    runWithPackageDB $ loadModules filePaths >> descInter
  where
    descInter =
      setTopLevelModules ["Description"] >>
        interpret ("description " ++ show picPath ++ parens taskData) infer



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
  => String       -- ^ Data available for making the description
  -> String       -- ^ Additional code module
  -> String       -- ^ Module containing the /description/ function
  -> FilePath     -- ^ Path images will be stored in
  -> IO (LangM m) -- ^ `OutputCapable` representation of task description
validDescription taskData globalModule descModule picPath = do
  let fileName = hash $ descModule ++ taskData ++ globalModule
  cDir <- cacheDir
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
      res <- makeDescription taskData globalModule descModule picPath
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
If the syntax check fail, then no semantics feedback is provided.
Semantics feedback is coupled with a rating given as a Rational (0 to 1).
-}
checkSolution
  :: String   -- ^ Data made available to checker functions
  -> String   -- ^ Additional code module
  -> String   -- ^ Module containing /parseSubmission/
  -> String   -- ^ Module containing /checkSyntax/ and /checkSemantics/
  -> String   -- ^ Student solution
  -> FilePath -- ^ Path images will be stored in
  -> IO (Either InterpreterError ([Output], Maybe (Maybe Rational, [Output])))
checkSolution taskData globalCode parseCode checkCode submission picPath = do
    filePaths <- writeUncachedAndGetPaths
      [ ("Global", globalCode)
      , ("Parse", parseCode)
      , ("Check", checkCode)
      ]
    runWithPackageDB (loadModules filePaths >> runCheck) >>= sequence
  where
    runCheck = do
      setImportsQ
        [ ("Control.OutputCapable.Blocks.Type", Just "OC")
        , ("Data.Either", Just "E")
        ]
      setTopLevelModules ["Check", "Parse"]
      interpret checkSyntaxAbort infer

    checkSyntaxAbort = unlines $
      "do" :
      indent (
       "let" :
       indent
        [ "isAbort (OC.Refuse _)          = True"
        , "isAbort (OC.Assertion False _) = True"
        , "isAbort _                      = False"
        , "parsed = " <> parseSubmission
        , "syn = " <> "either (refuse . code . show)" <>
           parens (check "checkSyntax") <> "parsed"
        ]
       ++
       [ "synRes <- OC.getOutputSequence syn"
       , "if any isAbort synRes"
       , "  then"
       , "    pure (synRes,Nothing)"
       , "  else do"
       , "    let sem = " <> check "checkSemantics" <>
              "(E.fromRight undefined parsed)"
       , "    semRes <- OC.getOutputSequenceWithRating sem"
       , "    pure (synRes, Just semRes)"
       ])

    indent = map ("  " ++)

    check func = func <> parens taskData <> show picPath

    parseSubmission =
        "parseSubmission " <>
        removeUnicodeEscape (show $ replace "\\\\" "\\" submission)



writeUncachedAndGetPaths :: [(String, String)] -> IO [FilePath]
writeUncachedAndGetPaths xs = do
    paths <- getCachePaths xs
    writeUncachedFiles paths
    pure $ map fst paths
  where
    getCachePaths :: [(String,String)] -> IO [(FilePath,String)]
    getCachePaths files = do
      dir <- cacheDir
      pure $ map (\(prefix, content) ->
                 (dir </> prefix <> "-" <> hash content <.> "hs",content)) files

    writeUncachedFiles :: [(FilePath,String)] -> IO ()
    writeUncachedFiles = void . mapM (\ (path,content) ->
      doesFileExist path >>= flip unless (writeFile path content))



extract :: Either InterpreterError c -> c
extract = either (error . show) id


hash :: Show a => a -> String
hash = showDigest . sha256 . encodeUtf8 . pack . show



cacheDir :: IO FilePath
cacheDir = do
  temporary <- getTemporaryDirectory
  let dir = temporary </> "FlexCache"
  createDirectoryIfMissing False dir
  pure dir



imageLinks :: [Output] -> [FilePath]
imageLinks = concatMap gatherLinks
  where
    gatherLinks :: Output -> [FilePath]
    gatherLinks (Image l)        = [l]
    gatherLinks (Images m)       = elems m
    gatherLinks (Assertion _ os) = imageLinks os
    gatherLinks (Paragraph os)   = imageLinks os
    gatherLinks (Refuse os)      = imageLinks os
    gatherLinks (Enumerated os)  = imageLinks $ concat together
      where
        together = concatMap (\(a,b) -> [a,b]) os
    gatherLinks (Itemized oss)   = imageLinks $ concat oss
    gatherLinks (Indented os)    = imageLinks os
    gatherLinks _                = []
