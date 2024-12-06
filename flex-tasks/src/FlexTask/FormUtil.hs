{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TypeOperators #-}

{- | Functions for creating and composing forms.
-}

module FlexTask.FormUtil
  (
  -- * Functions for Rendered
    ($$>)
  , addCss
  , addJs
  , addCssAndJs
  , applyToWidget
  , getFormData
  -- * Convenience functions for Yesod FieldSettings
  , addAttribute
  , addAttributes
  , addCssClass
  , addNameAndCssClass
  , supportedLanguages
  -- * functions for custom forms
  , newFlexId
  , newFlexName
  , repeatFlexName
  , standaloneDefaultsJS
  ) where


import Control.Monad.Reader            (runReader)
import Data.Containers.ListUtils       (nubOrd)
import Data.Map                        (Map, fromList)
import Data.Text                       (Text, pack, unpack)
import Data.Text.Lazy                  (toStrict)
import Data.Tuple.Extra                (second)
import System.Log.FastLogger           (defaultBufSize, newStdoutLoggerSet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Cassius                    (Css)
import Text.Julius                     (Javascript, RawJS(..))
import Yesod
import Yesod.Core.Types                (RY)
import Yesod.Default.Config2           (makeYesodLogger)

import qualified Control.Monad.Trans.RWS as RWS   (get)
import qualified Data.Text               as T     (replace)
import qualified Yesod.Core.Unsafe       as Unsafe

import FlexTask.YesodConfig (
  FlexForm(..),
  Handler,
  Rendered,
  Rendered',
  Widget,
  )




{- |
Compose two forms sequentially.
The output form contains all of the fields from both input forms.
-}
infixr 0 $$>
($$>) :: Monad m => Rendered' m -> Rendered' m -> Rendered' m
f1 $$> f2 = do
    res1 <- f1
    res2 <- f2
    pure $ do
      (names1,wid1) <- res1
      (names2,wid2) <- res2
      pure (nubOrd $ names1 ++ names2, wid1 >> wid2)


applyToWidget :: Functor m => (Widget -> Widget) -> Rendered' m -> Rendered' m
applyToWidget f form = fmap (second f) <$> form


addContent
  :: (ToWidget FlexForm (render -> a), Functor m)
  => (render -> a)
  -> Rendered' m
  -> Rendered' m
addContent content = applyToWidget (<* toWidget content)


{- |
Add CSS to a form.
Use with `Yesod` Cassius or Lucius Shakespeare quasi quoters or hosted files.
-}
addCss
  :: (render ~ RY FlexForm, Functor m)
  => (render -> Css) -- ^ CSS template
  -> Rendered' m     -- ^ Form to add to
  -> Rendered' m
addCss = addContent


{- |
Add JavaScript to a form.
Use with `Yesod` Julius Shakespeare quasi quoters or hosted files.
-}
addJs
  :: (render ~ RY FlexForm, Functor m)
  => (render -> Javascript) -- ^ Javascript template
  -> Rendered' m            -- ^ Form to add to
  -> Rendered' m
addJs = addContent


{- |
Like `addCss` and `addJs`, but for including CSS and JavaScript in one step.
-}
addCssAndJs
  :: (render ~ RY FlexForm, Functor m)
  => (render -> Css)        -- ^ CSS template
  -> (render -> Javascript) -- ^ Javascript template
  -> Rendered' m            -- ^ Form to add to
  -> Rendered' m
addCssAndJs css js = applyToWidget ((<* toWidget css) . (<* toWidget js))


{- |
Convenience function to directly create a Yesod FieldSetting with this name and CSS Class.
-}
addNameAndCssClass :: Text -> Text -> FieldSettings app
addNameAndCssClass name cssClass = addFieldAttrs
  where
    fSettings = fieldSettingsLabel name
    addFieldAttrs = fSettings {
      fsName = Just name,
      fsAttrs = addClass cssClass $ fsAttrs fSettings
      }


-- | Add an attribute-value pair to the given FieldSettings
addAttribute :: (Text,Text) -> FieldSettings app -> FieldSettings app
addAttribute attribute fs =  fs { fsAttrs = attribute : fsAttrs fs}


-- | Add a list of attribute-value pairs to the given FieldSettings
addAttributes :: [(Text,Text)] -> FieldSettings app -> FieldSettings app
addAttributes as fs =  fs { fsAttrs = as ++ fsAttrs fs}


-- | Add a CSS class to the given FieldSettings
addCssClass :: Text -> FieldSettings app -> FieldSettings app
addCssClass c fs = fs { fsAttrs = addClass c $ fsAttrs fs}


{- |
Get a unique identifier for an html element.
The format is "flexident[number]"
-}
newFlexId :: MForm Handler Text
newFlexId = T.replace "h" "flex" <$> newIdent


-- | repeat the last received name.
repeatFlexName :: MForm Handler Text
repeatFlexName = do
  i <- RWS.get
  pure $ pack $ "flex" ++ show i


{- |
Get a unique name for an html element.
The format is "flex[number]"
-}
newFlexName :: MForm Handler Text
newFlexName = T.replace "f" "flex" <$> newFormIdent


standaloneDefaultsJS :: [Text] -> Text
standaloneDefaultsJS ts =
  "<script>" <>
  toStrict (renderJavascriptUrl (\_ _ -> undefined) $ setDefaultsJS ts) <>
  "</script>"


setDefaultsJS :: [Text] -> JavascriptUrl url
setDefaultsJS names = [julius|
function setDefaults(values){
  for(let i = 0; i < fieldNames.length; i++){
    var input = values[i];
    var fields = document.getElementsByName(fieldNames[i]);

    for(let j = 0; j < fields.length; j++) {
      var field = fields[j];
      var fieldType = field.getAttribute("type");
      var maybeDropdown = field.tagName;

      if(fieldType != null && fieldType.toLowerCase() === "radio"){
        field.checked = field.value == input;
      }
      else if(maybeDropdown != null && maybeDropdown.toLowerCase() === "select"){
        for(const opt of Array.from(field.options)){
          opt.selected = input.includes(opt.getAttribute("value"));
        }
      }
      else if(fieldType != null && fieldType.toLowerCase() === "checkbox"){
        field.checked = input.includes(field.getAttribute("value"));
      }
      else{
        var inputElem = fields.length > 1 ? JSON.parse(input)[j] : input;
        if(inputElem != "Missing" && inputElem != "None"){
          field.value = inputElem;
        }
      }
    }
  }
}
var fieldNames = #{rawJS (show names)};|]


-- | List of languages to cover in instances of `RenderMessage` for custom translations.
supportedLanguages :: [Lang]
supportedLanguages = ["de","en"]


{- |
Extract a form from the environment.
The result is an IO embedded tuple of field IDs and a map of language and internationalized html pairs.
-}
getFormData :: Rendered -> IO ([String], Map Lang String)
getFormData widget = do
    logger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    Unsafe.fakeHandlerGetLogger
      appLogger
      FlexForm {appLogger = logger}
      writeHtml
  where
    writeHtml :: Handler ([String], Map Lang String)
    writeHtml = case supportedLanguages of
      (l:ls) -> do
        (names,first) <- withLang l
        rest <- traverse (fmap snd . withLang) ls
        return (map unpack names, fromList $ first:rest)
      _ -> error "No supported languages found!"

    withLang :: Lang -> Handler ([Text], (Lang, String))
    withLang lang = do
      setLanguage lang
      (names,wid) <- fst <$> runFormGet (runReader widget)
      clearSession
      let withJS = wid >> toWidgetBody (setDefaultsJS names)
      content <- widgetToPageContent withJS
      html <- withUrlRenderer [hamlet|
        ^{pageHead content}
        ^{pageBody content}|]
      return (names, (lang, concat $ lines $ renderHtml html))
