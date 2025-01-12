{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module State (
    AuthStatus (..),
    AuthError (..),
    AppState (..),
    Screen (..),
    initialState,
    authStatus,
    currentScreen,
    tokenInput,
) where

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified GitHub.Auth as GH
import Lens.Micro.TH (makeLenses)

data Screen
    = MainMenu
    | LoginScreen
    | DemoScreen -- TODO: What should the demo screen look like?
    deriving (Eq, Show)

data AuthError
    = InvalidToken
    | NetworkError String
    | UnknownError String
    deriving (Eq, Show)

data AuthStatus
    = NotAuthenticated
    | Authenticating
    | AuthFailed AuthError
    | Authenticated GH.Auth -- Store the auth token
    deriving (Eq, Show)

data AppState = AppState
    { _authStatus :: AuthStatus
    , _currentScreen :: Screen
    , _tokenInput :: E.Editor T.Text ()
    }
makeLenses ''AppState

initialState :: AppState
initialState =
    AppState
        { _authStatus = NotAuthenticated
        , _currentScreen = MainMenu
        , _tokenInput = E.editor () (Just 1) ""
        }
