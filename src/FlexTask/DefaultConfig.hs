{-# Language QuasiQuotes #-}

module FlexTask.DefaultConfig (defaultConfig) where


import Text.RawString.QQ (rQ)

import FlexTask.Types (FlexConf(..))




defaultConfig :: FlexConf
defaultConfig = FlexConf dGlobalDefs dTaskAndForm dParse dCheckTemplate



dGlobalDefs :: String
dGlobalDefs = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Definitions inside this module can be imported everywhere.
Adjust the solution type or add utility functions here.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}
module Global where


type Solution = (Int,Int)

|]


-- REMINDER: REWRITE THIS USAGE MANUAL!
dTaskAndForm :: String
dTaskAndForm = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
generate the solution, task description and form here.
supply a function 'getTask',
which returns a tuple of task description, interpolated feedback functions and input form.

If no special form is required you may use
the generic function 'formify' to generate a form for you.
Derive Generic for your solution type and add
an instance of Formify without a body.
the expression 'formify fieldNames [OPTIONAL DEFAULT VALUE(S) or Nothing]' will then generate a form
with synced field names.
When using a custom form, consider that the field names must match the actual form.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}
{-# language ApplicativeDo #-}
{-# language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# Language QuasiQuotes #-}
module TaskAndForm where


import Control.OutputCapable.Blocks
import Control.OutputCapable.Blocks.Generic.Type (
    GenericOutput
    )
import Data.String.Interpolate (i)
import Data.Text               (Text)
import FlexTask.FormUtil       (getFormData)
import FlexTask.Generic.Form
import FlexTask.YesodConfig    (Rendered)
import GHC.Generics            (Generic)
import Test.QuickCheck.Gen

import qualified Data.Text as T

import Global


genNumbers = vectorOf 3 $ elements ([1..6] :: [Int])


getTask :: OutputCapable m => FilePath -> Gen (LangM m, String, IO ([String],String))
getTask _ = do
    numbers <- genNumbers
    let sol = (product numbers, sum numbers)
        description = output (numbers !! 0) (numbers !! 1) (numbers !! 2)
    pure (description, interpolate sol, getFormData form)



output :: OutputCapable m => Int -> Int -> Int -> LangM m
output one two three = do
  paragraph $ translate $ do
    german "Ich würfle drei Zahlen."
    english "I throw a die three times."
  paragraph $ translate $ do
    german [i|Die erste ist #{one}, die zweite ist #{two}, die letzte ist #{three}.|~]
    english [i|On the first throw I roll #{one}, the second #{two} and on the third one #{three}.|~]
  indent $ paragraph $ translate $ do
    german "Was ist die Summe dieser Zahlen?"
    english "What is the sum of these numbers?"
  pure ()


fieldNames :: [[FieldInfo]]
fieldNames = [[single "Product"], [single "Sum"]]


form :: Rendered
form = formify (Nothing :: Maybe Solution) fieldNames

|]



-- REMINDER: REWRITE THIS USAGE MANUAL!
dParse :: String
dParse = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Parse the solution given as a String here.
Supply a function 'parseSubmission',
which parses the input and returns your desired solution type.
If no special parsing is required, you can do as above and
generate a parser by using the generic function parseInput.
Instantiate your type for the type class Parse and derive
Generic for it to use this feature.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}
{-# language ApplicativeDo #-}
{-# language DeriveGeneric #-}
module ParseAndCheck where


import Data.String.Interpolate (i)
import Data.Text               (Text)
import FlexTask.Generic.Parse  (parseInput)
import GHC.Generics            (Generic)
import Text.Parsec             (ParseError, parse)

import qualified Data.Text as T

import Global




parseSubmission :: String -> Either ParseError Solution
parseSubmission = parse parseInput ""

|]



dCheckTemplate :: String
dCheckTemplate = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Evaluate the parsed solution here.
edit this interpolation function to create a checker for syntax
as well as semantics.
You may import additional modules in this template.
The interpolated code will be inserted into the above module
'ParseAndCheck'. Therefore, every import from that module is
also in scope for this code snippet. Please avoid directly
conflicting imports inside this template and the above module.

Keep in mind that slashes must be escaped.
I.e. when writing an anonymous function, write
'\\x -> ...' instead of '\x -> ...' to avoid compile failure.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}
interpolate solution = [i|


import Control.OutputCapable.Blocks
import Control.OutputCapable.Blocks.Generic.Type (GenericOutput)
import Data.Ratio


checkSyntax :: OutputCapable m => FilePath -> Either ParseError Solution -> LangM m
checkSyntax _ (Left err) = refuse $ code $ show err
checkSyntax _ (Right try)
  | try == sol = pure ()
  | otherwise =
      refuse $ indent $ translate $ do
        german "syntaktisch falsch"
        english "syntactically wrong"


checkSemantics :: OutputCapable m => FilePath -> Either ParseError Solution -> Rated m
checkSemantics _ (Left err) = refuse (code $ show err) *> pure 0.0
checkSemantics _ (Right try)
  | try == sol = pure 1.0
  | otherwise = do
      refuse $ indent $ translate $ do
        german "semantisch falsch"
        english "semantically wrong"
      pure 0.0


sol = #{solution}
|~]

|]
