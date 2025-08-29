{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}

module FlexTask.ConvertForm (
  getFormData,
  ) where


import qualified Yesod.Core.Unsafe      as Unsafe

import Control.Monad.Reader             (runReader)
import Data.Map                         (fromList)
import Data.IORef                       (readIORef, writeIORef)
import Data.Text                        (Text)
import System.Log.FastLogger            (defaultBufSize, newStdoutLoggerSet)
import Text.Blaze.Html.Renderer.String  (renderHtml)
import Yesod
import Yesod.Core.Types                 (HandlerData(..), HandlerFor(..), ghsIdent)
import Yesod.Default.Config2            (makeYesodLogger)

import FlexTask.Types                   (HtmlDict)
import FlexTask.Processing.Text         (supportedLanguages)
import FlexTask.YesodConfig             (FlexForm(..), Handler, Rendered, Widget)



-- reset internal id generator to have same ids in all languages
resetIdentGen :: HandlerFor master ()
resetIdentGen = do
    x <- HandlerFor $ readIORef . handlerState
    HandlerFor $ flip writeIORef x {ghsIdent = 0} . handlerState


{- |
Extract a form from the environment.
The result is an IO embedded tuple of field IDs and a map of language and internationalized html pairs.
-}
getFormData :: Rendered Widget -> IO ([Text], HtmlDict)
getFormData widget = do
    logger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    Unsafe.fakeHandlerGetLogger
      appLogger
      FlexForm {appLogger = logger}
      writeHtml
  where
    writeHtml :: Handler ([Text], HtmlDict)
    writeHtml = case supportedLanguages of
      (l:ls) -> do
        (names,first) <- withLang l
        rest <- traverse (fmap snd . withLang) ls
        return (names, fromList $ first:rest)
      _ -> error "No supported languages found!"

    withLang :: Lang -> Handler ([Text], (Lang, String))
    withLang lang = setRequestLang lang $ do
      resetIdentGen
      (names,wid) <- fst <$> runFormGet (runReader widget)
      content <- widgetToPageContent wid
      html <- withUrlRenderer [hamlet|
        ^{pageHead content}
        ^{pageBody content}|]
      return (names, (lang, concat $ lines $ renderHtml html))



-- Manipulate the request data to use a specific language.
setRequestLang :: Lang -> HandlerFor master a -> HandlerFor master a
setRequestLang lang HandlerFor{..} = do
  HandlerFor $ unHandlerFor . alterHandlerData
  where
    alterHandlerData hd@HandlerData{..} =
      hd{handlerRequest = handlerRequest{reqLangs = [lang]}}
