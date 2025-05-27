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


setDefaultsJS :: [[Text]] -> JavascriptUrl url
setDefaultsJS names = [julius|
  function setDefaults(values) {
    const handlers = {
      radio:   (field, value) => { field.checked = field.value == value; },
      select:  (field, value) => {
        Array.from(field.options).forEach(option => {
          if (Array.isArray(value)) {
            option.selected = value.includes(option.value);
          } else {
            option.selected = option.value === value;
          }
        });
      },
      checkbox:(field, value) => {
        if (Array.isArray(value)) {
          field.checked = value.includes(field.value);
        } else {
          field.checked = field.value == value;
        }
      },
      default: (field, value) => {
        if (value !== "Missing" && value !== "None") {
          field.value = value;
        }
      }
    };

    const getHandler = field => {
      const type = field.getAttribute("type")?.toLowerCase();
      const tag  = field.tagName.toLowerCase();
      if (type === "radio")    return "radio";
      if (tag  === "select")   return "select";
      if (type === "checkbox") return "checkbox";
      if (type === "hidden")   return null;  // skip hidden
      return "default";
    };

    fieldNames.forEach((names, i) => {
      const input = values[i];

      if (names.length > 1) {
        names.forEach((fieldName, j) => {
          // extract the corresponding sub-value
          let value;
          if (Array.isArray(input)) {
            value = input[j];
          } else {
            value = JSON.parse(input)[j];
          }

          document.getElementsByName(fieldName).forEach(field => {
            const key = getHandler(field);
            if (key) handlers[key](field, value);
          });
        });
      } else {
        const fieldName = names[0];
        const value     = input;
        document.getElementsByName(fieldName).forEach(field => {
          const key = getHandler(field);
          if (key) handlers[key](field, value);
        });
      }
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
          .forEach(elem => {
            if (elem.getAttribute("type")?.toLowerCase() === "radio" ||
                elem.getAttribute("type")?.toLowerCase() === "checkbox" ||
                elem.tagName.toLowerCase() === "select"){
              elem.disabled = true;
            }
            else {
              elem.readOnly = true;
            }
          });
      });
    };|]
  | otherwise = mempty
