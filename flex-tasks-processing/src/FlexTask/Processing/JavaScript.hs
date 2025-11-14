{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module FlexTask.Processing.JavaScript (
  setDefaultsJS,
  triggerDefaults,
  lockForm,
  triggerLockForm,
  ) where


import Data.Text                        (Text)
import Text.Julius                      (JavascriptUrl, julius, rawJS)

import FlexTask.Processing.Text         (formatForJS)


setDefaultsJS :: JavascriptUrl url
setDefaultsJS = [julius|
  function setDefaults(fieldNames, values) {
    const handlers = {
      radio:    (field, value) => { field.checked   = field.value == value; },
      select:   (field, value) => {
        Array.from(field.options).forEach(opt => {
          if (Array.isArray(value)) {
            opt.selected = value.includes(opt.value);
          } else {
            opt.selected = (opt.value === value);
          }
        });
      },
      checkbox: (field, value) => {
        if (Array.isArray(value)) {
          field.checked = value.includes(field.value);
        } else {
          field.checked = (field.value == value);
        }
      },
      default:  (field, value) => {
        if (value !== "Missing" && value !== "None") {
          field.value = value;
        }
      }
    };

    const getHandler = field => {
      const t = field.getAttribute("type")?.toLowerCase();
      const tag = field.tagName.toLowerCase();
      if (t === "radio")    return "radio";
      if (tag === "select") return "select";
      if (t === "checkbox") return "checkbox";
      if (t === "hidden")   return null;  // skip hidden
      return "default";
    };

    if (fieldNames.length == values.length) {
      fieldNames.forEach((names, i) => {
        const raw = values[i];

        if (names.length > 1) {
          names.forEach((name, j) => {
            let val = Array.isArray(raw) ? raw[j] : JSON.parse(raw)[j];
            document.getElementsByName(name).forEach(field => {
              const key = getHandler(field);
              if (key) handlers[key](field, val);
            });
          });
        }
        else {
          const name = names[0];
          const fields = Array.from(document.getElementsByName(name));
          const isList = Array.isArray(raw) || /^\[\"/.test(raw);

          fields.forEach((field, j) => {
            let val;
            const key = getHandler(field);
            if (isList) {
              const arr = Array.isArray(raw) ? raw : JSON.parse(raw);
              if (key === "checkbox" || key === "select") {
                val = arr;
              }
              else {
                val = arr[j];
              }
            }
            else {
              val = raw;
            }
            if (key) handlers[key](field, val);
          });
        }
      });
    }
  }
|]


triggerDefaults :: [[Text]] -> Text -> JavascriptUrl url
triggerDefaults names values = [julius|
  window.onload = setDefaults(#{rawJS (show names)}, #{rawJS (formatForJS values)});
|]


lockForm :: JavascriptUrl url
lockForm = [julius|
  function lockForm(fieldNames) {
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


triggerLockForm :: [[Text]] -> JavascriptUrl url
triggerLockForm fieldNames = [julius|window.onload = lockForm(#{rawJS $ show $ concat fieldNames});|]
