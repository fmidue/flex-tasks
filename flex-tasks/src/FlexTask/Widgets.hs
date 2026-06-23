{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module FlexTask.Widgets where


import Control.Monad (unless, forM_)
import Control.Monad.Reader (reader)
import Data.List (find)
import Data.Text (Text)
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
      return ([ident],[[name]],widget)



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
<input id=#{theId}-none type=radio name=#{name} value="None" :isSel:checked>
|]
    nothingFun _ optionWidget =
      let emptyRadio = [whamlet|
$newline never
<label>
  ^{optionWidget}
  _{MsgSelectNone}
|]
      in [whamlet|
$newline never
$if isVertical
  <div>
    ^{emptyRadio}
$else
  ^{emptyRadio}
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
  :: Eq a
  => Handler (OptionList a)
  -> Field Handler a
selectField = dropdownHelper
    (\theId name attrs isReq inside -> [whamlet|
$newline never
<select ##{theId} name=#{name} :isReq:required *{attrs}>
  $if isReq
    <option value="" selected disabled>_{MsgSelectNone}
  ^{inside}
|]) -- outside
    (\_theId _name isSel -> [whamlet|
$newline never
<option value="None" :isSel:selected>_{MsgSelectNone}
|]) -- when optional
    (\_theId _name _attrs value isSel text -> toWidget [whamlet|
$newline never
<option value=#{value} :isSel:selected>#{text}
|]) -- inside
    (Just $ \label -> [whamlet|
<optgroup label=#{label}>
|]) -- group label


{- |
Modification of the Yesod.Forms `selectFieldHelper` function.
This forces the user to actively pick an option in required dropdowns
instead of defaulting to one of the options.
-}
dropdownHelper
  :: Eq a
  => (Text -> Text -> [(Text, Text)] -> Bool -> Widget -> Widget)
  -> (Text -> Text -> Bool -> Widget)
  -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetFor FlexForm ())
  -> Maybe (Text -> Widget)
  -> Handler (OptionList a)
  -> Field Handler a
dropdownHelper outside onOpt inside groupHeader opts' = Field
  { fieldParse = undefined
  , fieldView = \theId name attrs val isReq -> do
      outside theId name attrs isReq $ do
        optsFlat <- olOptions.flattenOptionList <$> handlerToWidget opts'
        unless isReq $ onOpt theId name $ render optsFlat val `notElem` map optionExternalValue optsFlat
        opts'' <- handlerToWidget opts'
        case opts'' of
          OptionList {} -> constructOptions theId name attrs val isReq optsFlat
          OptionListGrouped {olOptionsGrouped = groups} -> do
                forM_ groups $ \(grp, opts) -> do
                  case groupHeader of
                    Just header -> header grp
                    Nothing -> return ()
                  constructOptions theId name attrs val isReq opts
  , fieldEnctype = UrlEncoded
  }
  where
    flattenOptionList (OptionListGrouped os re) = OptionList (concatMap snd os) re
    flattenOptionList ol = ol
    render _ (Left x) = x
    render opts (Right a) = maybe "" optionExternalValue $ find ((== a) . optionInternalValue) opts
    constructOptions theId name attrs val isReq opts =
      forM_ opts $ \opt ->
        inside
        theId
        name
        ((if isReq then (("required", "required"):) else id) attrs)
        (optionExternalValue opt)
        (render opts val == optionExternalValue opt)
        (optionDisplay opt)
