{-# language TypeFamilies #-}

module FlexTask.YesodConfig
  ( FlexForm(..)
  , Handler
  , Widget
  ) where


import Yesod
import Yesod.Core.Types (Logger)




newtype FlexForm = FlexForm {
  appLogger :: Logger
  }


type Handler = HandlerFor FlexForm
type Widget = WidgetFor FlexForm ()



instance Eq (Route FlexForm) where
  (==) _ _ = True


instance RenderRoute FlexForm where
  data Route FlexForm
  renderRoute _ = ([],[])


instance Yesod FlexForm


instance RenderMessage FlexForm FormMessage where
  renderMessage _ _ = defaultFormMessage
