{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module FlexTask.FormUtilSpec where


import Data.Text                        (Text)
import GHC.Generics                     (Generic)
import Test.Hspec (
  Spec,
  anyErrorCall,
  describe,
  it,
  )
import Text.Cassius                     (Css, cassius)
import Text.Julius                      (JavascriptUrl, julius)

import FlexTask.ConvertForm
import FlexTask.TestUtil                (shouldNotThrow, shouldReturnSame)
import FlexTask.FormUtil
import FlexTask.Generic.Form



data TestEnum = First | Last
  deriving (Bounded,Enum,Eq, Formify, Generic)


spec :: Spec
spec = do
  let
    form1 = formify @(Int,[Text]) Nothing $
      basic "test" >|
      list Vertical basicField ["test2","test3"]
    form2 = formify @(Maybe Text) Nothing $ basic "form2"
    form3 = formify (Just $ MultipleChoice [First]) $
      multipleChoiceEnum Dropdown "form3"
        (\a -> if a == First then "first" else "last")

  describe "getFormData" $
    it "does not throw an error for Formify generated test forms" $ do
      getFormData form1 `shouldNotThrow` anyErrorCall
      getFormData form2 `shouldNotThrow` anyErrorCall
      getFormData form3 `shouldNotThrow` anyErrorCall
  describe "($$>))" $
    it "should be associative" $
        getFormData ((form1 $$> form2) $$> form3)
        `shouldReturnSame`
        getFormData (form1 $$> (form2 $$> form3))
  describe "addCss and addJs" $
    it "are order invariant" $
      getFormData (addJs testJs $ addCss testCss form1)
      `shouldReturnSame`
      getFormData (addCss testCss $ addJs testJs form1)
  describe "addCssAndJs" $
    it "should behave the same as applying CSS and JS one after the other" $
      getFormData (addCssAndJs testCss testJs form3)
      `shouldReturnSame`
      getFormData (addCss testCss $ addJs testJs form3)



testCss :: render -> Css
testCss = [cassius|
  p
    text-align: center
    color: red
|]

testJs :: JavascriptUrl url
testJs = [julius|
  myFunction(){
    console.log("test");
  }
|]
