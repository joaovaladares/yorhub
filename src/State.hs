{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module State (
    AuthStatus (..),
    AuthError (..),
    AppState (..),
    Screen (..),
    RepoLoadStatus (..),
    initialState,
    authStatus,
    currentScreen,
    tokenInput,
    repoInput,
    username,
    animationFrame,
    repoStatus,
) where

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified GitHub.Auth as GH

import GH.Repo (RepoError, RepoInfo)
import Lens.Micro.TH (makeLenses)
import UI.Types

data Screen
    = AuthScreen -- Token input
    | RepoSelectionScreen -- Repo input
    | RepoDetailScreen
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

data RepoLoadStatus
    = NotLoaded
    | Loading
    | LoadFailed RepoError
    | Loaded RepoInfo
    deriving (Eq, Show)

data AppState = AppState
    { _authStatus :: AuthStatus
    , _currentScreen :: Screen
    , _animationFrame :: Int
    , _tokenInput :: E.Editor T.Text Name
    , _repoInput :: E.Editor T.Text Name
    , _username :: Maybe T.Text
    , _repoStatus :: RepoLoadStatus
    }
makeLenses ''AppState

initialState :: AppState
initialState =
    AppState
        { _authStatus = NotAuthenticated
        , _currentScreen = AuthScreen
        , _animationFrame = 0
        , _tokenInput = E.editor TokenEditor (Just 1) ""
        , _repoInput = E.editor RepoEditor (Just 1) ""
        , _username = Nothing
        , _repoStatus = NotLoaded
        }
