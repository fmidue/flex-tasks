{-# language QuasiQuotes #-}

module FlexTask.FormUtil
  ( getFormData
  ) where


import Data.Text             (Text)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Text.Blaze.Internal   (Markup)
import Text.Julius           (RawJS(..))
import Yesod
import Yesod.Default.Config2 (makeYesodLogger)

import qualified Yesod.Core.Unsafe as Unsafe

import FlexTask.YesodConfig  (FlexForm(..), Handler, Widget)




setDefaultsJS :: [Text] -> JavascriptUrl url
setDefaultsJS names = [julius|
function setDefaults(values){
  for(let i = 0; i < values.length; i++){
    var input = values[i];
    var fieldId = fieldIds[i];
    if(input != "Missing" && input != "None"){
      var element = document.getElementById(fieldId);
      var maybeRadio = document.getElementById(fieldId + "-1");

      if(maybeRadio != null && maybeRadio.getAttribute("type").toLowerCase() == "radio"){
         document.getElementById(fieldId + "-" + input).checked = true;
      }

      if(element.tagName.toLowerCase() == "select"){
        for(const opt of Array.from(element.options)){
          if(input.includes(opt.getAttribute("value"))){
            opt.selected = true;
          }
        }
      }

      if(document.getElementsByName(fieldId)[0].getAttribute("type").toLowerCase() == "checkbox"){
        for(const box of document.getElementsByName(fieldId)){
          if(input.includes(box.getAttribute("value"))){
            box.checked = true;
          }
        }
      }
      else{
        element.value = input;
      }
    }
  }
}
var fieldIds = #{rawJS (show names)};|]



getFormData :: (Markup -> MForm Handler ([Text],Widget)) -> IO ([Text],Html)
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
