{-# language QuasiQuotes #-}

module FlexTask.Widgets where



import Control.Monad.Reader (reader)
import Yesod

import FlexTask.FormUtil (
  newFlexId,
  newFlexName,
  )
import FlexTask.Styling     (horizontalRBStyle)
import FlexTask.YesodConfig (
  FlexForm,
  Handler,
  Rendered,
  Widget,
  )



renderForm
    :: (FieldSettings FlexForm -> AForm Handler a)
    -> FieldSettings FlexForm
    -> Rendered Widget
renderForm aformStub label =
    reader $ \fragment -> do
      ident <- maybe newFlexId pure $ fsId label
      name <- newFlexName
      let addAttrs = label {fsName = Just name, fsId = Just ident}
      (_, views') <- aFormToForm $ aformStub addAttrs
      let views = views' []
      let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <span :fvRequired view:.required :not $ fvRequired view:.optional .flex-form-span>
        <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
|]
      return ([[name]],widget)



joinWidgets :: [[Widget]] -> Widget
joinWidgets = mapM_ (insertDiv . sequence_)
  where
    insertDiv w = [whamlet|
      $newline never
      <div .flex-form-div>
        ^{w}
    |]



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
          outside theId _name _attrs inside' =
            toWidget horizontalRBStyle >> [whamlet|
$newline never
<div>
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



checkboxField :: Eq a => Bool -> Handler (OptionList a) -> Field Handler [a]
checkboxField isVertical optList = (multiSelectField optList)
      { fieldView =
          \theId title attrs val _isReq -> do
              os <- olOptions <$> handlerToWidget optList
              let selected (Left _) _ = False
                  selected (Right values) opt = optionInternalValue opt `elem` values
                  checkboxWidget opt = [whamlet|
<label>
  <input type=checkbox name=#{title} value=#{optionExternalValue opt} *{attrs} :selected val opt:checked>
  #{optionDisplay opt}
|]
              [whamlet|
<span ##{theId}>
  <input type=hidden name=#{title} value=0>
  $forall opt <- os
    $with box <- checkboxWidget opt
      $if isVertical
        <div>
          ^{box}
      $else
        ^{box}
|]
      }
