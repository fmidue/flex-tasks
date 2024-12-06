{-# language InstanceSigs #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskellQuotes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

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
  -- * adapted Yesod functionality
  -- $yesodFuncs
  , fwhamlet
  , getSessionLangs
  , toMForm
  ) where


import Control.Monad.Reader (Reader)
import qualified Control.Monad.Trans.RWS as RWS
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (
  Q,
  Exp (..),
  Pat (..),
  newName,
  )
import Text.Hamlet (
  hamletWithSettings,
  defaultHamletSettings,
  Env(..),
  HamletRules(..),
  )
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
type Rendered' m = m (MForm Handler ([Text],Widget))
-- | More specific version of Rendered using Html
type Rendered = Rendered' (Reader Html)


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


{-| $yesodFuncs
Internationalisation in Yesod relies on language data provided by the request.
The dummy application runs in a test environment using fake, empty request data.
As such, setting the language of Messages is only possible via user session.

The following functions are slight adaptations from Yesod.
They act the same as the standard library version,
but access the user session instead of the request to determine the used language.
-}


{- |
Like Yesod's whamlet QuasiQuoter for Widgets.
-}
fwhamlet :: QuasiQuoter
fwhamlet = hamletWithSettings flexWhamletRules defaultHamletSettings


-- Clone of normal whamlet rules, but doesn't use `getMessageRender` for Messages.
flexWhamletRules :: Q HamletRules
flexWhamletRules = do
    ah <- [|asWidgetT . toWidget|]
    let helper qg f = do
            x <- newName "uRender"
            e <- f $ VarE x
            let e' = LamE [VarP x] e
            g <- qg
            bind <- [|(>>=)|]
            return $ InfixE (Just g) bind (Just e')
    let ur f = do
            let env = Env
                    (Just $ helper [|getUrlRenderParams|])
                    (Just $ helper [|fmap (toHtml .) getMsgRender|])
            f env
    return $ HamletRules ah ur $ \_ b -> return $ ah `AppE` b


getMsgRender :: (MonadHandler m, RenderMessage (HandlerSite m) message) => m (message -> Text)
getMsgRender = renderMessage <$> getYesod <*> getSessionLangs


{- |
retrieve supported languages from the user session.
-}
getSessionLangs :: MonadHandler m => m [Text]
getSessionLangs = maybeToList <$> lookupSession "_LANG"


{- |
Like Yesod's aFormToForm, turning an applicative form into a monadic form.
-}
toMForm :: (Monad m, HandlerSite m ~ site, MonadHandler m)
            => AForm m a
            -> MForm m (FormResult a, [FieldView site] -> [FieldView site])
toMForm (AForm aform) = do
    ident <- RWS.get
    (env, site, _) <- RWS.ask    -- ignore request languages
    lang <- getSessionLangs  -- use languages stored in session
    (a, xml, ident', enc) <- lift $ aform (site, lang) env ident
    RWS.put ident'
    RWS.tell enc
    return (a, xml)
