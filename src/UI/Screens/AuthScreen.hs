module UI.Screens.AuthScreen (
    drawAuthScreen,
    handleAuthScreen,
) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Config (saveToken)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GH.Auth (validateToken)
import GH.User (getUserName)
import Lens.Micro ((^.))
import State
import UI.Types
import UI.Widgets.CommandBar

drawAuthScreen :: AppState -> B.Widget Name
drawAuthScreen st =
    B.vBox
        [ B.vLimit 1 $ B.str "YorHub"
        , B.hBorder
        , B.padAll 1 $ case st ^. authStatus of
            NotAuthenticated ->
                B.vBox
                    [ B.str "Welcome! Please enter your GitHub Personal Access Token to continue:"
                    , B.padTop (B.Pad 1) $
                        B.hLimit 50 $
                            B.vLimit 10 $
                                B.withAttr inputAttr $
                                    B.border $
                                        E.renderEditor (B.str . T.unpack . T.concat) True (st ^. tokenInput)
                    ]
            Authenticating ->
                B.center $ B.str "Authenticating..."
            AuthFailed err ->
                B.vBox
                    [ B.withAttr errorAttr $ B.str $ "Authentication failed: " ++ show err
                    , B.padTop (B.Pad 1) $ B.str "Press Enter to try again"
                    ]
            Authenticated _ ->
                B.center $ B.withAttr successAttr $ B.str "Authentication successful! Loading..."
        , B.fill ' '
        , B.hBorder
        , drawCommandBar st
        ]

handleAuthScreen :: V.Event -> B.EventM Name AppState ()
handleAuthScreen (V.EvKey V.KEsc []) = B.halt
handleAuthScreen (V.EvKey V.KEnter []) = do
    st <- B.get
    case st ^. authStatus of
        NotAuthenticated -> do
            let token = T.concat $ E.getEditContents $ st ^. tokenInput
            B.modify $ \s -> s{_authStatus = Authenticating}
            res <- liftIO $ validateToken token
            case res of
                Right auth -> do
                    liftIO $ saveToken token
                    userRes <- liftIO $ getUserName auth
                    case userRes of
                        Left err -> B.modify $ \s -> s{_authStatus = AuthFailed err}
                        Right name -> do
                            B.modify $ \s ->
                                s
                                    { _authStatus = Authenticated auth
                                    , _currentScreen = RepoSelectionScreen
                                    , _username = Just name
                                    }
                Left err -> B.modify $ \s -> s{_authStatus = AuthFailed err}
        AuthFailed _ ->
            B.modify $ \s -> s{_authStatus = NotAuthenticated}
        _ -> return ()
handleAuthScreen ev = B.zoom tokenInput $ E.handleEditorEvent (B.VtyEvent ev)
