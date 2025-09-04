{-# language InstanceSigs #-}
{-# language TypeFamilies #-}
{-# language PatternSynonyms #-}

{-|
Default Yesod configuration for form generating environment.
Also exports some convenient type synonyms hiding underlying complexity.
-}

module FlexTask.YesodConfig
  ( FlexForm(..)
  -- * Yesod type synonyms
  , Handler
  , Widget
  -- * Form type
  , Rendered'
  , Rendered
  -- * Convenience Patterns
  {- |
  Patterns for defining POST parameters in custom forms.
  Mostly for ease of reading/self documenting code.
  -}
  , pattern Singular
  , pattern Multiple
  ) where


import Control.Monad.Reader (Reader)
import Data.Text (Text)
import Yesod
import Yesod.Core.Types (Logger)




-- | Dummy Yesod application the form environment runs in.
newtype FlexForm = FlexForm {
  appLogger :: Logger
  }


-- | Form submits a lone POST parameter
pattern Singular :: Text -> [[Text]]
pattern Singular t = [[t]]

-- | Form submits multiple POST parameters
pattern Multiple :: [Text] -> [[Text]]
pattern Multiple ts = [ts]


-- |
type Handler = HandlerFor FlexForm
type Widget = WidgetFor FlexForm ()
-- | General type of composable forms inside the environment
type Rendered' m w = m (MForm Handler ([[Text]],w))
-- | More specific version of Rendered using Html
type Rendered w = Rendered' (Reader Html) w


instance Eq (Route FlexForm) where
  (==) :: Route FlexForm -> Route FlexForm -> Bool
  (==) _ _ = True


instance RenderRoute FlexForm where
  data Route FlexForm
  renderRoute _ = ([],[])


-- | Minimal definitions of Yesod typeclasses for `FlexForm`
instance Yesod FlexForm


instance RenderMessage FlexForm FormMessage where
  renderMessage _ _ = defaultFormMessage
