{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module FlexTask.FormUtil
  ( ($$>)
  , getFormData
  , newFlexId
  , newFlexName
  , repeatFlexName
  ) where


import Data.Text             (Text, pack)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Text.Julius           (RawJS(..))
import Yesod
import Yesod.Default.Config2 (makeYesodLogger)

import qualified Control.Monad.Trans.RWS as RWS   (get)
import qualified Data.Text               as T     (replace)
import qualified Yesod.Core.Unsafe       as Unsafe

import FlexTask.YesodConfig  (FlexForm(..), Handler, Rendered)




infixr 0 $$>
($$>) :: Rendered -> Rendered -> Rendered
first $$> second = do
    res1 <- first
    res2 <- second
    pure $ do
      (names1,wid1) <- res1
      (names2,wid2) <- res2
      pure (names1++names2, wid1 >> wid2)



newFlexId :: MForm Handler Text
newFlexId = T.replace "h" "flex" <$> newIdent



repeatFlexName :: MForm Handler Text
repeatFlexName = do
  i <- RWS.get
  pure $ pack $ "flex" ++ show i



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



getFormData :: Rendered -> IO ([Text],Html)
getFormData widget = do
    logger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    unsafeHandler FlexForm {appLogger = logger} writeHtml
  where
    unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

    writeHtml :: Handler ([Text],Html)
    writeHtml = do
      ((names,wid),_) <- runFormGet widget
      let withJS = wid >> toWidgetBody (setDefaultsJS names)
      content <- widgetToPageContent withJS
      html <- withUrlRenderer [hamlet|^{pageBody content}|]
      return (names,html)
