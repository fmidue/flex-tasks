{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module FlexTask.Processing.JavaScript (
  standaloneDefaultsJS,
  setDefaultsJS,
  ) where


import Data.Text                        (Text)
import Data.Text.Lazy                   (toStrict)
import Text.Julius (
  JavascriptUrl,
  julius,
  rawJS,
  renderJavascriptUrl,
  )



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
