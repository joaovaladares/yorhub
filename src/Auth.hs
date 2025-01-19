module Auth (
    initializeAuth,
) where

import GH.Auth (loadAndValidateToken)
import GH.User (getUserName)
import State

initializeAuth :: IO AppState
initializeAuth = do
    result <- loadAndValidateToken
    case result of
        Left _ -> return initialState
        Right auth -> do
            userRes <- getUserName auth
            let initialUser = either (const Nothing) Just userRes
            return
                initialState
                    { _authStatus = Authenticated auth
                    , _currentScreen = RepoSelectionScreen
                    , _username = initialUser
                    }
