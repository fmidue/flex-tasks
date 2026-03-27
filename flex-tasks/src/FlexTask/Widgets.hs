{-# language QuasiQuotes #-}

module FlexTask.Widgets where



import Control.Monad.Reader (reader)
import Yesod

import FlexTask.FormUtil (
  newFlexId,
  newFlexName,
  )
import FlexTask.Styling     (horizontalRBStyle, checkboxStyle)
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
      <div .flex-form-div .form-group>
        ^{w}
    |]



radioField :: Eq a => Bool -> Handler (OptionList a) -> Field Handler a
radioField isVertical = selectFieldHelper outside onOpt inside Nothing
  where
    outside theId _name _attrs inside' =
      toWidget horizontalRBStyle >> [whamlet|
$newline never
<div>
  <span ##{theId}>^{inside'}
|]
    onOpt theId name isSel = nothingFun theId [whamlet|
$newline never
<input id=#{theId}-none type=radio name=#{name} value="" :isSel:checked>
|]
    nothingFun theId optionWidget = [whamlet|
$newline never
<.radio>
  ^{optionWidget}
  <label for=#{theId}-none>
    _{MsgSelectNone}
|]
    inside theId name attrs value isSel display =
      let radio = [whamlet|
$newline never
<label>
  <input id=#{theId}-#{(value)} type=radio name=#{name} value=#{(value)} :isSel:checked *{attrs}>
  \#{display}
|]
      in [whamlet|
$newline never
$if isVertical
  <div>
    ^{radio}
$else
  ^{radio}
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
              toWidget checkboxStyle >> [whamlet|
<div ##{theId}>
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


selectField
  :: (Eq a, RenderMessage site FormMessage)
  => Bool
  -> HandlerFor site (OptionList a)
  -> Field (HandlerFor site) a
selectField req = selectFieldHelper
    (\theId name attrs inside -> [whamlet|
$newline never
<select ##{theId} name=#{name} :req:required *{attrs}>
$if req
  <option value="" selected disabled>_{MsgSelectNone}
^{inside}
|]) -- outside
    (\_theId _name isSel -> [whamlet|
$newline never
<option value="None" :isSel:selected>_{MsgSelectNone}
|]) -- onOpt
    (\_theId _name _attrs value isSel text -> toWidget [whamlet|
$newline never
<option value=#{value} :isSel:selected>#{text}
|]) -- inside
    (Just $ \label -> [whamlet|
<optgroup label=#{label}>
|]) -- group label
