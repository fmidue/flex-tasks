{-# Language QuasiQuotes #-}


module FlexTask.DefaultConfig (defaultConfig) where


import Text.RawString.QQ (rQ)

import FlexTask.Types (CommonModules(..), FlexConf(..))




-- | Simple task configuration with textual user guide
defaultConfig :: FlexConf
defaultConfig =
  FlexConf {
    taskDataModule = dTaskData,
    commonModules = CommonModules dGlobalDefs dSettings dDescription dParse []
  }



dGlobalDefs :: String
dGlobalDefs = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Module for shared definitions. Can be imported in any other segment.
Adjust the submission and task data types or add utility types and functions here.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}

module Global where


type Submission = (Int,Int)
type DescData = (Int,Int,Int)
type TaskData = (DescData,Submission)
|]



dSettings :: String
dSettings = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Module for configuration constants. Can be imported in any other segment.
It is not recommended to import this in the check template.
Instead, use interpolation to directly embed the used values.
See the section on `Check` for more information.

Define a constant `validateSettings` which checks the values defined in this module for validity.
It can be defined as `pure ()` if no checks are required.
`validateSettings` is meant for sanity checks preventing the lecturer from
choosing inappropriate settings during task configuration.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}
module TaskSettings where


import Control.OutputCapable.Blocks (LangM, OutputCapable)


validateSettings :: OutputCapable m => LangM m
validateSettings = pure ()
|]

dTaskData :: String
dTaskData = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Module for generating static task data, possibly randomized.
This includes (in order):

- Flexible data available to both the task description and the checks/feedback. (TaskData)
- The entire "Check" module, containing a syntax and a semantics check. (String, use QuasiQuoting)
- An HTML input form represented by the names of all contained input fields
  and a map of languages to translated HTML code, wrapped in IO. (IO ([Text],HtmlDict))

Provide a function
getTask :: Gen (TaskData, String, IO ([Text],HtmlDict))
implementing a generator for these elements.

If no specific form is required, you may use 'formify' to generate a generic form for you,
based on the type of your input.
Derive Generic for your submission type and add an instance of Formify without a body. The expression

formify {OPTIONAL DEFAULT VALUE(S) OR Nothing} STRUCTURE

will then generate a form with synchronised field names.
See the 'FlexTask.Generic.Form' documentation for more information.

You can also create custom forms, if something more sophisticated is required.
The forms are normal Yesod MForms that are wrapped in a Reader Monad.
The Reader models the insertion of a security HTML fragment for POST and GET forms
that is automatically performed when rendering the form.

To write your own form, simply design an MForm first, as described in the Yesod book.
Then replace the returned FormResult with a list of field names.
Make sure that the field names match the names used in the actual form.
Finally, run the form in the reader Monad using the following expression (import 'reader' from 'Control.Monad.Reader'):

reader $ \frag -> YOURFORM

and interpolate the HTML argument at the top of the contained widget:

...
#{frag}
...

You can compose multiple forms using the '$$>' infix function.
This will mostly be necessary when using a generated form together with a custom one.
Apply 'getFormData' to your finished form to obtain the data for the generator.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}

{-# language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module TaskData (getTask) where


import FlexTask.FormUtil       (getFormData)
import FlexTask.Generic.Form
import FlexTask.Types          (HtmlDict)
import FlexTask.YesodConfig    (Rendered, Widget)
import Data.String.Interpolate (i)
import Data.Text               (Text)
import Test.QuickCheck.Gen
import Yesod                   (RenderMessage(..), fieldSettingsLabel)

import Global




data Label = Product | Sum



instance RenderMessage a Label where
  renderMessage _ ("de":_) Product = "Produkt"
  renderMessage _ ("de":_) Sum     = "Summe"
  renderMessage _ _        Product = "Product"
  renderMessage _ _        Sum     = "Sum"



getTask :: Gen (TaskData, String, IO ([Text],HtmlDict))
getTask = do
    numbers@(n1,n2,n3) <- (,,) <$> intInRange <*> intInRange <*> intInRange
    let checkData = (product [n1,n2,n3], sum [n1,n2,n3])
    pure ((numbers,checkData), checkers, getFormData form)
  where
    intInRange = chooseInt (1,6)


fieldNames :: [[FieldInfo]]
fieldNames = [[fromLabel Product], [fromLabel Sum]]
  where
    fromLabel = single. fieldSettingsLabel


form :: Rendered Widget
form = formify (Nothing :: Maybe Submission) fieldNames



{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Use this function to create the 'Check' module.
The entire module is first created as a String.
It will later be written to file as an actual module.
This module must contain two functions:

checkSyntax :: OutputCapable m => FilePath -> TaskData -> Submission -> LangM m

checkSemantics :: OutputCapable m => FilePath -> TaskData -> Submission -> Rated m

TaskData is the actual type of the flexible data generated above.
It is stored in the task instance and passed to both check functions.
Used to store variable data affected by random generation (part of the data might
not be needed in the checkers but in the Description module discussed below).
Submission is the actual type of the student submission after parsing and possibly post-processing.
The FilePath argument is the server path for storing and loading images and other data.
It is supplied by the caller of the checker functions and can be used as is, if file creation is required.
The type signature must also be adjusted in this case.
A 'MonadIO m' constraint must be set for file operations to work (including an import of the type class).
Otherwise, the FilePath argument can be completely ignored.

The functions' result types are taken from 'Control.OutputCapable.Blocks'.
They model a type-independent representation of checks and corresponding feedback.
Refer to the library's documentation for help.
LangM is feedback without a score.
Rated is feedback with a final score as a fraction, i.e. 0 to 1.

As this function produces a String, you can also use interpolation.
Use that to precompute data and interpolate the results directly into the module.
This is most useful for static data that is not affected by random generation, and expensive calculations.
Note that slashes must be escaped.
I.e. when introducing an anonymous function, write
'\\x -> ...' instead of '\x -> ...' to avoid compile failure.

checkSyntax will always run first.
If it does not succeed, then checkSemantics will not be evaluated for the submission.
checkSemantics is only run if checkSyntax finished successfully,
i.e. no syntax error was found in the given submission.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}

checkers :: String
checkers = [i|


{-\# language ApplicativeDo \#-}

module Check (checkSyntax, checkSemantics) where


import Control.OutputCapable.Blocks

import Global


checkSyntax :: OutputCapable m => FilePath -> TaskData -> Submission -> LangM m
checkSyntax _ _ _  = pure ()  -- nothing to check here


checkSemantics :: OutputCapable m => FilePath -> TaskData -> Submission -> Rated m
checkSemantics _ (_,sol) try
  | try == sol = pure 1.0
  | otherwise = do
      refuse $ indent $ translate $ do
        german "semantisch falsch"
        english "semantically wrong"
      pure 0.0

|~]
|]


dDescription :: String
dDescription = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Module for writing the task description.
This needs to contain the function

description :: OutputCapable m => FilePath -> TaskData -> LangM m

where FilePath, TaskData and LangM are as explained in the 'Check' module.
This function is also supplied with static task data, the same as the check functions.
If different data is required, consider splitting the type into a tuple of two different contents,
one for each of the modules.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}

{-# Language ApplicativeDo #-}
{-# Language QuasiQuotes #-}

module Description (description) where


import Control.OutputCapable.Blocks
import Data.String.Interpolate                   (i)

import Global




description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ ((one,two,three),_) = do
  paragraph $ translate $ do
    german "Ich würfle drei Zahlen."
    english "I throw a die three times."
  paragraph $ translate $ do
    german [i|Die erste ist #{one}, die zweite ist #{two}, die letzte ist #{three}.|~]
    english [i|On the first throw I roll #{one}, the second #{two} and on the third one #{three}.|~]
  indent $ paragraph $ translate $ do
    german "Was sind Summe und Produkt dieser Zahlen?"
    english "What are the sum and the product of these numbers?"
  pure ()
|]


dParse :: String
dParse = [rQ|
{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Module for parsing the student submission.
Must contain the function

parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission

where the given String is the submission.
This function should first apply parsing to the submission,
then embed the result into 'OutputCapable'.
The type 'LangM' (ReportT o m) Submission' is a specialization of the more general 'LangM' m Submission'.
'LangM' m Submission' represents sequential output like 'LangM m' or 'Rated m',
but provides a value of type Submission afterwards.
The function thus enables more complex reporting (e.g., of errors)
than might be possible by purely using basic parsers alone.
The final result is passed to the check functions to generate feedback.

The parsers used throughout are those of 'Text.Parsec'.
Refer to its documentation if necessary.

To implement parseSubmission, you will typically invoke `parseInfallibly` or 'parseWithOrReport'
and 'reportWithFieldNumber', and possibly also 'parseWithFallback', all
supplied by 'FlexTask.Generic.Parse'. Simply using `parseInfallibly formParser`
or `parseWithOrReport formParser reportWithFieldNumber` directly
reads the form inputs and embeds the result directly into 'OutputCapable'.
That is enough if you do not need additional processing of the input.
If you do, '<&>' may suffice in simple situations, or else use additional
calls to 'parseWithOrReport' or 'parseWithFallback' to additionally parse/process
Strings from among the form result, that is, individual input fields.
'parseWithFallback' takes a parser, messaging function, fallback parser and the input.
The secondary parser is used as a simpler sanity check on the input in case
of an error with the primary parser.
The possible error of the fallback parser and the original error
are then fed to the messaging function to construct the report.
Use this to produce more sophisticated error messages.
A simpler route is to also use just 'parseWithOrReport' in this phase, with
something like 'const (text . show)' as the reporting function.

If you want to chain multiple parsing steps, e.g. with 'parseWithFallback',
use '$>>=' of 'Control.OutputCapable.Blocks.Generic'.
This operation can be seen as a '>>=' equivalent for 'LangM''.
Example:
```
parseWithOrReport formParser reportWithFieldNumber input
  $>>= \s -> parseWithFallback p someFunc fallback s
    $>>= pure . ...
```

As with forms, a generic parser interface is available.
The steps are similar:
Derive Generic for your data type and include an instance declaration of 'Parse' without an implementation.
It is also possible to write your own, using standard Parsec functions.
The submission has a specific encoding, which is already taken into account by automatically generated parsers.
When using a custom parser, you need to include the encoding or your parser will fail.
The 'escaped' function from 'FlexTask.Generic.Parse' takes a parser and reads the encoding around it.
This ensures that your custom parser will work as expected.

If your submission type consists of types with generic parsers, as well as custom parsers,
do not write a custom parser for the whole type.
Instead, use bodyless instances for the component types where possible
and use custom parsers for those where not applicable.
Finally, use the bodyless instance method for the entire type.
This is again necessary to avoid encoding problems that are caused internally by argument delimiters.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}

module Parse (parseSubmission) where


import Control.OutputCapable.Blocks (
  LangM',
  ReportT,
  OutputCapable,
  )
import FlexTask.Generic.Parse  (formParser, parseWithOrReport, reportWithFieldNumber)

import Global



parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission
parseSubmission = parseWithOrReport formParser reportWithFieldNumber


{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
You can add arbitrarily many additional modules after this point.
They must be delimited by at least three `=` as before.
These modules can be imported freely.
Make sure their names do not overlap with those above.
Also avoid the name `Helper`, which is already used internally.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}
|]
