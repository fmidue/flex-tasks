{-# language PatternSynonyms #-}

module FlexTask.Interpreter
  ( checkSolution
  , genFlexInst
  , runWithPackageDB
  , writeUncachedAndGetPaths
  ) where


import Control.Monad                (unless, void)
import Control.Monad.IO.Class       (liftIO)
import Control.OutputCapable.Blocks (LangM, ReportT)
import Control.OutputCapable.Blocks.Type (
      Output,
      pattern Assertion,
      pattern Refuse,
      getOutputSequence,
      getOutputSequenceWithRating
      )
import Data.Digest.Pure.SHA         (sha256, showDigest)
import Data.List                    (isPrefixOf)
import Data.List.Extra              (replace)
import Data.List.Split              (split, whenElt)
import Data.Tuple.Extra             (both)
import Data.Text.Lazy.Encoding      (encodeUtf8)
import Data.Text.Lazy               (pack)
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




type GenOutput = (LangM (ReportT Output IO), String, IO ([String],String))


genFlexInst
  :: FlexConf
  -> (Gen GenOutput -> a -> GenOutput)
  -> a
  -> FilePath
  -> IO FlexInst
genFlexInst
  (FlexConf global taskAndForm parse checkTemplate)
  genMethod
  seed
  picPath
  = do
      filePaths <- writeUncachedAndGetPaths
        [ ("Global", global)
        , ("TaskAndForm", joinCode taskAndForm checkTemplate)
        ]
      taskAndFormResult <- runWithPackageDB $
                             loadModules filePaths >> tfInter
      let gen = extract taskAndFormResult
      let (output, iCheck, io) = genMethod gen seed
      (fields,html) <- io
      desc <- getOutputSequence output
      let parseAndCheck = joinCode parse iCheck
      void $ writeUncachedAndGetPaths [("ParseAndCheck", parseAndCheck)]
      pure $ FlexInst desc fields html global parseAndCheck
    where
      extract = either (error . show) id

      tfInter :: Interpreter (Gen GenOutput)
      tfInter = setTopLevelModules ["TaskAndForm"] >>
                  interpret ("getTask " ++ show picPath) infer



runWithPackageDB :: Interpreter a -> IO (Either InterpreterError a)
runWithPackageDB interpreter = do
  path <- getEnv "FLEX_PKGDB"
  unsafeRunInterpreterWithArgs ["-package-db " <> path] interpreter



checkSolution
    :: FlexInst
    -> String
    -> FilePath
    -> IO (Either InterpreterError ([Output], Maybe (Maybe Rational, [Output])))
checkSolution
  (FlexInst _ _ _ globalCode parseAndCheckCode)
  submission
  picPath
  = do
    filePaths <- writeUncachedAndGetPaths
      [ ("Global", globalCode)
      , ("ParseAndCheck", parseAndCheckCode)
      ]
    runWithPackageDB $ loadModules filePaths >> runCheck
  where
    runCheck = do
      let arguments = show picPath <> parseSolution
      setTopLevelModules ["ParseAndCheck"]
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



writeUncachedAndGetPaths :: [(String,String)] -> IO [FilePath]
writeUncachedAndGetPaths xs = do
    paths <- getCachePaths xs
    writeUncachedFiles paths
    pure $ map fst paths
  where
    hash :: Show a => a -> String
    hash = showDigest . sha256 . encodeUtf8 . pack . show

    cacheDir :: IO FilePath
    cacheDir = do
      temporary <- getTemporaryDirectory
      let dir = temporary </> "FlexCache"
      createDirectoryIfMissing False dir
      pure dir

    getCachePaths :: [(String,String)] -> IO [(FilePath,String)]
    getCachePaths files = do
      dir <- cacheDir
      pure $ map (\(prefix, content) ->
                 (dir </> prefix <> "-" <> hash content <.> "hs",content)) files

    writeUncachedFiles :: [(FilePath,String)] -> IO ()
    writeUncachedFiles = void . mapM (\ (path,content) ->
      doesFileExist path >>= flip unless (writeFile path content))



joinCode :: String -> String -> String
joinCode code1 code2 =
  let
    (imports1, restCode1) = splitImports code1
  in
    imports1 <> code2 <> restCode1



splitImports :: String -> (String,String)
splitImports code =
    both (unlines . concat) $ splitAt (length splitOff -1) splitOff
  where
    splitOff = (split . whenElt) (isPrefixOf "import") $ lines code
