{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module FlexTask.Processing.JavaScript (
  setDefaultsJS,
  triggerDefaults,
  lockForm,
  ) where


import Data.Text                        (Text)
import Text.Julius                      (JavascriptUrl, julius, rawJS)
import qualified Data.Text as T

import FlexTask.Processing.Text         (formatForJS)


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
      else if(fieldType != null && fieldType.toLowerCase() !== "hidden"){
        var inputElem = fields.length > 1 ? JSON.parse(input)[j] : input;
        if(inputElem != "Missing" && inputElem != "None"){
          field.value = inputElem;
        }
      }
    }
  }
}
var fieldNames = #{rawJS (show names)};|]


triggerDefaults :: Text -> JavascriptUrl url
triggerDefaults t
  | t == "[ ]" || T.length t < 2 = mempty
  | otherwise = [julius|window.onload = setDefaults(#{rawJS (formatForJS t)});|]


lockForm :: Bool -> JavascriptUrl url
lockForm lock
  | lock = [julius|window.onload =
      function () {
        for(const name of fieldNames) {
          for(const elem of document.getElementsByName(name)){
            elem.disabled = true;
          }
        }
      };|]
  | otherwise = mempty
