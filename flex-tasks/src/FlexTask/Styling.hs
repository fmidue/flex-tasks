{-# LANGUAGE QuasiQuotes #-}

module FlexTask.Styling where


import Text.Cassius (Css, cassius)



horizontalRBStyle :: render -> Css
horizontalRBStyle = [cassius|
  input[type="radio"]
    margin-left: 20px;
    margin-right: 5px;
  |]
