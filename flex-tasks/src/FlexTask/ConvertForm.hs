{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}

module FlexTask.ConvertForm (
  getFormData,
  unsafeGetFormData,
  ) where


import qualified Yesod.Core.Unsafe      as Unsafe

import Control.Monad.Reader             (runReader)
import Data.Map                         (fromList)
import Data.IORef                       (readIORef, writeIORef)
import Data.Text                        (Text)
import System.IO.Unsafe                 (unsafePerformIO)
import System.Log.FastLogger            (defaultBufSize, newStdoutLoggerSet)
import Text.Blaze.Html.Renderer.String  (renderHtml)
import Yesod
import Yesod.Core.Types                 (HandlerData(..), HandlerFor(..), ghsIdent)
import Yesod.Default.Config2            (makeYesodLogger)

import FlexTask.Types                   (HtmlDict)
import FlexTask.Processing.Text         (supportedLanguages)
import FlexTask.YesodConfig             (FlexForm(..), Handler, Rendered, Widget)



-- reset internal id generator to have same ids in all languages
resetIdentGen :: Handler ()
resetIdentGen = do
    x <- HandlerFor $ readIORef . handlerState
    HandlerFor $ flip writeIORef x {ghsIdent = 0} . handlerState


{- |
Extract a form from the environment.
The result is an IO embedded tuple of field IDs and a map of language and internationalized html pairs.
-}
getFormData :: Rendered Widget -> IO ([[Text]], HtmlDict)
getFormData widget = do
    logger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    Unsafe.fakeHandlerGetLogger
      appLogger
      FlexForm {appLogger = logger}
      writeHtml
  where
    writeHtml :: Handler ([[Text]], HtmlDict)
    writeHtml = case supportedLanguages of
      (l:ls) -> do
        (names,first) <- withLang l
        rest <- traverse (fmap snd . withLang) ls
        return (names, fromList $ first:rest)
      _ -> error "No supported languages found!"

    withLang :: Lang -> Handler ([[Text]], (Lang, String))
    withLang lang = setRequestLang lang $ do
      resetIdentGen
      (names,wid) <- fst <$> runFormGet (runReader widget)
      content <- widgetToPageContent wid
      html <- withUrlRenderer [hamlet|
        ^{pageHead content}
        ^{pageBody content}|]
      return (names, (lang, concat $ lines $ renderHtml html))


{- |
Extract a form from the environment.
The result is a tuple of field IDs and a map of language and internationalized html pairs.

__This function employs `unsafePerformIO`!__
It should never be used if `getFormData` is applicable instead.

Intended to be used on non-randomized Autotool tasks (called "direct") where an IO context is not available.

Generally, usage of this should be safe if the `Rendered Widget` argument does not incorporate `liftIO` calls.
This will always be the case for non-custom forms.
For custom forms, the user is responsible for making sure such calls are avoided or considered "safe".
-}
unsafeGetFormData :: Rendered Widget -> ([[Text]], HtmlDict)
unsafeGetFormData = unsafePerformIO . getFormData


-- Manipulate the request data to use a specific language.
setRequestLang :: Lang -> Handler a -> Handler a
setRequestLang lang HandlerFor{..} = do
  HandlerFor $ unHandlerFor . alterHandlerData
  where
    alterHandlerData hd@HandlerData{..} =
      hd{handlerRequest = handlerRequest{reqLangs = [lang]}}
