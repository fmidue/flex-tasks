
{- |
Functions using `Interpreter` to run time compile and evaluate various aspects of a task.
The interpreted code is supplied by `FlexInst` or `FlexConf` data.
-}

module FlexTask.Interpreter
  ( checkSolution
  , genFlexInst
  , runWithPackageDB
  , validDescription
  ) where


import Control.Monad                (unless, void)
import Control.Monad.IO.Class       (liftIO)
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

import FlexTask.Types              (FlexConf(..), FlexInst(..))
import FlexTask.Processing.Text    (removeUnicodeEscape)




type GenOutput = (String, String, IO ([String],String))


{- |
Use a `FlexConf` to generate a `FlexInst`.
Interprets `taskDataCode` to generate the input form and description data.
Apply the given method to run the generator with a seed.
-}
genFlexInst
  :: FlexConf
  -> (Gen GenOutput -> a -> GenOutput) -- ^ Method of running the random generator
  -> a                                 -- ^ Generator seed
  -> IO FlexInst
genFlexInst
  (FlexConf global taskData description parse)
  genMethod
  seed
  = do
      filePaths <- writeUncachedAndGetPaths
        [ ("Global", global)
        , ("TaskData", taskData)
        ]
      taskAndFormResult <- runWithPackageDB $
                             loadModules filePaths >> tfInter
      let gen = extract taskAndFormResult
      let (descData, iCheck, io) = genMethod gen seed
      (fields,html) <- io
      pure $ FlexInst fields html descData global description parse iCheck
    where
      tfInter :: Interpreter (Gen GenOutput)
      tfInter = setTopLevelModules ["TaskData"] >>
                  interpret "getTask " infer



makeDescription
  :: (OutputCapable m, Typeable m)
  => FlexInst
  -> FilePath
  -> IO (Either InterpreterError (LangM m))
makeDescription (FlexInst _ _ descData global description _ _) picPath = do
    filePaths <- writeUncachedAndGetPaths
          [ ("Global", global)
          , ("Description", description)
          ]
    runWithPackageDB $ loadModules filePaths >> descInter
  where
    descInter =
      setTopLevelModules ["Description"] >>
        interpret ("description " ++ show picPath ++ " $ " ++ descData) infer



{- |
Produce the task description via caching.
Should the solution not yet exist on disc, then it will be interpreted and saved in a file.
If the description already exists on disc, it is read.
Then, if any of the image links of that description are invalid (have been deleted),
the description is interpreted again to regenerate the missing files.
-}
validDescription
  :: OutputCapable m
  => FlexInst
  -> FilePath     -- ^ Path images will be stored in
  -> IO (LangM m) -- ^ `OutputCapable` representation of task description
validDescription inst picPath = do
  let fileName = hash $  descriptionModule inst ++ descriptionData inst
  cDir <- cacheDir
  let path = cDir </> fileName
  b <- doesFileExist path
  if b
    then do
      output <- read <$> readFile path
      let fileLinks = imageLinks output
      exist <- mapM doesFileExist fileLinks
      if and exist
        then
          return $ toOutputCapable output
        else
          makeDescAndWrite path
    else
      makeDescAndWrite path
  where
    makeDescAndWrite p = do
      res <- makeDescription inst picPath
      output <- getOutputSequence $ extract res
      writeFile p $ show output
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
Interpret `parseModule` and `checkModule` to evaluate a submission.
The result is a tuple of syntax feedback and optional semantics feedback.
If the syntax check fail, then no semantics feedback is provided.
Semantics feedback is coupled with a rating given as a Rational (0 to 1).
-}
checkSolution
    :: FlexInst
    -> String   -- ^ Student solution
    -> FilePath -- ^ Path images will be stored in
    -> IO (Either InterpreterError ([Output], Maybe (Maybe Rational, [Output])))
checkSolution
  (FlexInst _ _ _ globalCode _ parseCode checkCode)
  submission
  picPath
  = do
    filePaths <- writeUncachedAndGetPaths
      [ ("Global", globalCode)
      , ("Parse", parseCode)
      , ("Check", checkCode)
      ]
    runWithPackageDB $ loadModules filePaths >> runCheck
  where
    runCheck = do
      let arguments = show picPath <> parseSolution
      setTopLevelModules ["Check"]
      first <- interpret
        ("checkSyntax" <> arguments)
        infer
      synRes <- liftIO $ getOutputSequence first
      if any isAbort synRes
        then pure (synRes,Nothing)
        else do
          second <- interpret
            ("checkSemantics" <> arguments)
            infer
          semRes <- liftIO $ getOutputSequenceWithRating second
          pure (synRes, Just semRes)

    isAbort (Refuse _)          = True
    isAbort (Assertion False _) = True
    isAbort _                   = False

    parseSolution = " $ parseSubmission "
                 <> removeUnicodeEscape (show $ replace "\\\\" "\\" submission)



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
