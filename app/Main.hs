module Main (main) where

import qualified Brick as B
import Config (loadToken)
import Control.Monad (void)
import GH.Auth (validateToken)
import GH.User (getUserName)
import State
import UI.App

main :: IO ()
main = do
    savedToken <- loadToken
    st <- case savedToken of
        Just token -> do
            result <- validateToken token
            case result of
                Left _ -> pure initialState
                Right auth -> do
                    userRes <- getUserName auth
                    let initialUser = either (const Nothing) Just userRes
                    pure
                        initialState
                            { _authStatus = Authenticated auth
                            , _currentScreen = RepoSelectionScreen
                            , _username = initialUser
                            }
        Nothing ->
            pure initialState
    void $ B.defaultMain app st
