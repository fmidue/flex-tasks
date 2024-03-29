{-# language QuasiQuotes #-}

module FlexTask.Widgets where


import FlexTask.YesodConfig (Handler, Widget)
import Yesod




renderFlatOrBreak :: Bool -> AForm Handler a -> Html -> MForm Handler Widget
renderFlatOrBreak lBreak aform fragment = do
    (_, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <span :fvRequired view:.required :not $ fvRequired view:.optional>
        <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
$if lBreak
  <div>
|]
    return widget



horizontalRadioField :: Eq a => Handler (OptionList a) -> Field Handler a
horizontalRadioField = withRadioFieldFlat
      (\theId optionWidget -> [whamlet|
$newline never
<div .radio>
    <label for=#{theId}-none>
      ^{optionWidget}
      _{MsgSelectNone}
|])
      (\theId value _isSel text optionWidget -> [whamlet|
$newline never
<label for=#{theId}-#{value}>
  ^{optionWidget}
  \#{text}
|])
  where
    withRadioFieldFlat nothingFun optFun =
      selectFieldHelper outside onOpt inside Nothing
        where
          outside theId _name _attrs inside' = [whamlet|
$newline never
<span ##{theId}>^{inside'}
|]
          onOpt theId name isSel = nothingFun theId [whamlet|
$newline never
<input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
|]
          inside theId name attrs value isSel display =
            optFun theId value isSel display [whamlet|
<input id=#{theId}-#{(value)} type=radio name=#{name} value=#{(value)} :isSel:checked *{attrs}>
|]



verticalCheckboxesField :: Eq a => Handler (OptionList a) -> Field Handler [a]
verticalCheckboxesField optList = (multiSelectField optList)
      { fieldView =
          \theId title attrs val _isReq -> do
              os <- olOptions <$> handlerToWidget optList
              let optSelected (Left _) _ = False
                  optSelected (Right values) opt = optionInternalValue opt `elem` values
              [whamlet|
<span ##{theId}>
  $forall opt <- os
    <div>
      <label>
        <input type=checkbox name=#{title} value=#{optionExternalValue opt} *{attrs} :optSelected val opt:checked>
        #{optionDisplay opt}
|]
      }
