{-# language TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

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
  ) where


import Control.Monad.Reader (Reader)
import Data.Text (Text)
import Yesod
import Yesod.Core.Types (Logger)




-- | Dummy Yesod application the form environment runs in.
newtype FlexForm = FlexForm {
  appLogger :: Logger
  }


-- |
type Handler = HandlerFor FlexForm
type Widget = WidgetFor FlexForm ()
-- | General type of composable forms inside the environment
type Rendered' m w = m (MForm Handler ([Text],w))
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
