{-# language QuasiQuotes #-}

module FlexTask.Widgets where



import Control.Monad.Reader (reader)
import Data.Text            (Text)
import Yesod

import FlexTask.FormUtil    (newFlexId, newFlexName, repeatFlexName)
import FlexTask.YesodConfig (FlexForm, Handler, Rendered)



renderFlatOrBreak
    :: Bool
    -> Bool
    -> (FieldSettings FlexForm -> AForm Handler a)
    -> Text
    -> Rendered
renderFlatOrBreak lBreak newId aformStub label =
    reader $ \fragment -> do
      ident <- newFlexId
      name <- if newId then newFlexName else repeatFlexName
      let addAttrs = (fieldSettingsLabel label)
                       {fsName = Just name, fsId = Just ident}
      (_, views') <- aFormToForm $ aformStub addAttrs
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
      return ([name],widget)



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
