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
      const raw = values[i];
      const fields = Array.from(document.getElementsByName(fieldName));
      let isList = false;
      let input = raw;

      try {
        const parse = JSON.parse(raw);
        if (Array.isArray(parse)) {
          input = parse;
          isList = true;
        }
      }
      catch {
        // proceed with 'input = raw' if parse fails
      }

      const handlers = {
        radio: (field, val) => {
          field.checked = field.value == val;
        },
        checkbox:(field, val) => {
          field.checked = Array.isArray(val)
            ? val.includes(field.value)
            : field.value == val;
        },
        select: (field, val) => {
          Array.from(field.options).forEach(opt => {
            opt.selected = Array.isArray(val)
              ? val.includes(opt.value)
              : opt.value == val;
          });
        },
        single: (field, val) => {
          if (val !== "Missing" && val !== "None") {
            field.value = val;
          }
        }
      };

      function pickKey(field) {
        const type = field.getAttribute("type")?.toLowerCase();
        const tag = field.tagName.toLowerCase();
        if (type === "radio") return "radio";
        if (type === "checkbox") return "checkbox";
        if (tag === "select") return "select";
        if (type === "hidden") return null; // Skip hidden fields
        return "single";
      }

      if (isList) {
        input.forEach((val, idx) => {
          const field = fields[idx];
          if (!field || val === "Missing" || val === "None") return;
          const key = pickKey(field);
          handlers[key](field, val);
        });
      }
      else {
        fields.forEach(field => {
          const key = pickKey(field);
          handlers[key](field, input);
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
            if (elem.getAttribute("type")?.toLowerCase()=== "radio" || elem.tagName.toLowerCase === "select"){
              elem.disabled = true;
            }
            else {
              elem.readOnly = true;
            }
          });
      });
    };|]
  | otherwise = mempty
