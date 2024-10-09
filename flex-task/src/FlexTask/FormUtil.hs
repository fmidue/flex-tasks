{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

{- | Functions for creating and composing forms.
-}

module FlexTask.FormUtil
  ( ($$>)
  , addContent
  , addCssAndJs
  , getFormData
  , newFlexId
  , newFlexName
  , repeatFlexName
  ) where


import Control.Monad.Reader            (runReader)
import Data.Text                       (Text, pack, unpack)
import Data.Tuple.Extra                (second)
import System.Log.FastLogger           (defaultBufSize, newStdoutLoggerSet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Julius                     (RawJS(..))
import Yesod
import Yesod.Default.Config2           (makeYesodLogger)

import qualified Control.Monad.Trans.RWS as RWS   (get)
import qualified Data.Text               as T     (replace)
import qualified Yesod.Core.Unsafe       as Unsafe

import FlexTask.YesodConfig  (FlexForm(..), Handler, Rendered, Rendered')




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
      pure (names1++names2, wid1 >> wid2)


{- |
Add additional content to a rendered form.
Use to include CSS and/or JavaScript via the usual `Yesod` Shakespeare methods.
A direct composition without using this function is also possible for custom forms.
-}
addContent :: (Functor m, ToWidget FlexForm a) => Rendered' m -> a -> Rendered' m
addContent form content = fmap (second (<* toWidget content)) <$> form


{- |
Add additional content to a rendered form.
Use to include CSS and/or JavaScript via the usual `Yesod` Shakespeare methods.
A direct composition without using this function is also possible for custom forms.
-}
addCssAndJs :: (Functor m, ToWidget FlexForm a, ToWidget FlexForm b) => Rendered' m -> a -> b -> Rendered' m
addCssAndJs form css js = fmap (second ((<* toWidget css) . (<* toWidget js))) <$> form


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


{- |
Extract a form from the environment.
The result is an IO embedded tuple of field IDs and Html code.
-}
getFormData :: Rendered -> IO ([String],String)
getFormData widget = do
    logger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    (fNames,html) <- unsafeHandler FlexForm {appLogger = logger} writeHtml
    let fields = unpack <$> fNames
    let form = concat $ lines $ renderHtml html
    return (fields,form)
  where
    unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

    writeHtml :: Handler ([Text],Html)
    writeHtml = do
      ((names,wid),_) <- runFormGet $ runReader widget
      let withJS = wid >> toWidgetBody (setDefaultsJS names)
      content <- widgetToPageContent withJS
      html <- withUrlRenderer [hamlet|
        ^{pageHead content}
        ^{pageBody content}|]
      return (names,html)
