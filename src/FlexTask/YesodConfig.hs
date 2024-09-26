{-# language TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module FlexTask.YesodConfig
  ( FlexForm(..)
  , Handler
  , Rendered
  , Rendered'
  , Widget
  ) where


import Control.Monad.Reader
import Data.Text (Text)
import Yesod
import Yesod.Core.Types (Logger)




newtype FlexForm = FlexForm {
  appLogger :: Logger
  }


type Handler = HandlerFor FlexForm
type Widget = WidgetFor FlexForm ()
type Rendered' m = m (MForm Handler ([Text],Widget))
type Rendered = Rendered' (Reader Html)


instance Eq (Route FlexForm) where
  (==) :: Route FlexForm -> Route FlexForm -> Bool
  (==) _ _ = True


instance RenderRoute FlexForm where
  data Route FlexForm
  renderRoute _ = ([],[])


instance Yesod FlexForm


instance RenderMessage FlexForm FormMessage where
  renderMessage _ _ = defaultFormMessage
