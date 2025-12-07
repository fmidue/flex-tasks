{-# LANGUAGE QuasiQuotes #-}

module FlexTask.Styling where


import Text.Cassius (Css, cassius)



horizontalRBStyle :: render -> Css
horizontalRBStyle = [cassius|
  input[type="radio"]
    margin-left: 15px;
    margin-right: 5px;
  |]

checkboxStyle :: render -> Css
checkboxStyle = [cassius|
  input[type="checkbox"]
    margin-left: 15px;
    margin-right: 5px;
  |]
