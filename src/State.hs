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
    repoInput,
    username,
) where

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified GitHub.Auth as GH

import Lens.Micro.TH (makeLenses)
import UI.Types

data Screen
    = AuthScreen -- Token input
    | RepoSelectionScreen -- Repo input
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
    | Authenticated GH.Auth
    deriving (Eq, Show)

data AppState = AppState
    { _authStatus :: AuthStatus
    , _currentScreen :: Screen
    , _tokenInput :: E.Editor T.Text Name
    , _repoInput :: E.Editor T.Text Name
    , _username :: Maybe T.Text
    }
makeLenses ''AppState

initialState :: AppState
initialState =
    AppState
        { _authStatus = NotAuthenticated
        , _currentScreen = AuthScreen
        , _tokenInput = E.editor TokenEditor (Just 1) ""
        , _repoInput = E.editor RepoEditor (Just 1) ""
        , _username = Nothing
        }
