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
  function setDefaults(values) {
    fieldNames.forEach((fieldName, i) => {
      const input = values[i];
      const fields = Array.from(document.getElementsByName(fieldName));

      // How to handle each field type
      const handlers = {
        radio: field => {
          field.checked = field.value == input;
        },
        select: field => {
          Array.from(field.options).forEach(option => {
            option.selected = input.includes(option.value);
          });
        },
        checkbox: field => {
          field.checked = input.includes(field.value);
        },
        default: (field, j) => {
          const inputElem = fields.length > 1 ? JSON.parse(input)[j] : input;
          if (inputElem !== "Missing" && inputElem !== "None") {
            field.value = inputElem;
          }
        }
      };

      // Pick a fitting handler based on field attributes
      const getHandler = field => {
        const type = field.getAttribute("type")?.toLowerCase();
        const tag = field.tagName.toLowerCase();
        if (type === "radio") return "radio";
        if (tag === "select") return "select";
        if (type === "checkbox") return "checkbox";
        if (type === "hidden") return null; // Skip hidden fields
        return "default";
      };

      fields.forEach((field, j) => {
        const key = getHandler(field);
        if (key && handlers[key]) {
          handlers[key](field, j);
        }
      });
    });
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
      fieldNames.forEach(name => {
        Array.from(document.getElementsByName(name))
          .forEach(elem => elem.disabled = true);
      });
    };|]
  | otherwise = mempty
