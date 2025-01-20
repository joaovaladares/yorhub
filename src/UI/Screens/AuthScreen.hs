module UI.Screens.AuthScreen (
    drawAuthScreen,
    handleAuthScreen,
    CustomEvent (..),
) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Widgets.Border.Style as BS
import Config (saveToken)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GH.Auth (validateToken)
import GH.User (getUserName)
import Lens.Micro ((%~), (&), (^.))
import State
import UI.Ascii
import UI.Types

drawAuthScreen :: AppState -> B.Widget Name
drawAuthScreen st =
    B.withBorderStyle BS.unicodeBold $
        B.border $
            B.hBox
                [ -- Left side with art and title
                  B.hLimit 65 $
                    B.vCenter $
                        B.vBox
                            [ B.withAttr asciiArtAttr $ B.hCenter $ B.str twoBAscii
                            , B.padAll 1 $
                                B.withAttr titleAttr $
                                    B.hCenter $
                                        B.str yorhubArt
                            ]
                , -- Right side with authentication content
                  B.vCenter $
                    B.vBox
                        [ B.withAttr systemAttr $ B.hCenter $ B.str "[SYSTEM]: Authentication Required"
                        , B.padTop (B.Pad 1) $ B.hCenter $ B.str "═══════════════════════════════════════════════"
                        , B.padTop (B.Pad 1) $
                            B.vBox
                                [ B.hCenter $ B.withAttr yorhaAttr $ B.str "> Operating as YoRHa Type B No.2"
                                , B.hCenter $ B.withAttr yorhaAttr $ B.str "> Requesting GitHub Access Authorization"
                                ]
                        , B.padTop (B.Pad 1) $ B.hCenter $ B.str "Paste your GitHub Personal Access Token:"
                        , B.padTop (B.Pad 1) $
                            B.hCenter $
                                B.hLimit 75 $
                                    B.withAttr inputAttr $
                                        B.border $
                                            E.renderEditor (B.str . T.unpack . T.concat) True (st ^. tokenInput)
                        , B.padTop (B.Pad 1) $
                            B.hCenter $
                                drawStatus (st ^. authStatus) (st ^. animationFrame)
                        , B.padTop (B.Pad 1) $
                            B.hCenter $
                                B.withAttr commandAttr $
                                    drawCommands (st ^. authStatus)
                        ]
                ]

-- Helper function to draw status based on authentication state
drawStatus :: AuthStatus -> Int -> B.Widget Name
drawStatus status frame =
    B.withAttr statusAttr $ B.str $ case status of
        NotAuthenticated ->
            "[STATUS]: Standing By..."
        Authenticating ->
            "[STATUS]: Authenticating " ++ [spinner !! (frame `mod` length spinner)]
        AuthFailed err ->
            "[STATUS]: Authentication Failed - " ++ show err
        Authenticated _ ->
            "[STATUS]: Authentication Successful"
  where
    spinner = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" -- Fancy spinner characters

-- Helper function to draw appropriate commands
drawCommands :: AuthStatus -> B.Widget Name
drawCommands status = B.str $ case status of
    NotAuthenticated ->
        "<ENTER> Execute Authentication  │  <ESC> Terminate Session"
    Authenticating ->
        "<ESC> Terminate Session"
    AuthFailed _ ->
        "<ENTER> Try Again  │  <ESC> Terminate Session"
    Authenticated _ ->
        "<ENTER> Continue  │  <ESC> Terminate Session"

data CustomEvent = AnimationTick

handleAuthScreen :: B.BrickEvent Name CustomEvent -> B.EventM Name AppState ()
handleAuthScreen (B.AppEvent AnimationTick) = do
    st <- B.get
    when (st ^. authStatus == Authenticating) $ do
        B.modify $ \s -> s & animationFrame %~ (\n -> (n + 1) `mod` 50)
        let currFrame = st ^. animationFrame
        when (currFrame == 25) $ do
            let token = T.concat $ E.getEditContents $ st ^. tokenInput
            res <- liftIO $ validateToken token
            case res of
                Right auth -> do
                    liftIO $ saveToken token
                    userRes <- liftIO $ getUserName auth
                    case userRes of
                        Left err -> B.modify $ \s -> s{_authStatus = AuthFailed err}
                        Right name ->
                            B.modify $ \s ->
                                s
                                    { _authStatus = Authenticated auth
                                    , _currentScreen = RepoSelectionScreen
                                    , _username = Just name
                                    }
                Left err -> B.modify $ \s -> s{_authStatus = AuthFailed err}
handleAuthScreen (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt
handleAuthScreen (B.VtyEvent (V.EvKey V.KEnter [])) = do
    st <- B.get
    case st ^. authStatus of
        NotAuthenticated -> do
            B.modify $ \s -> s{_authStatus = Authenticating}
        AuthFailed _ ->
            B.modify $ \s -> s{_authStatus = NotAuthenticated}
        _ -> return ()
handleAuthScreen (B.VtyEvent ev) = B.zoom tokenInput $ E.handleEditorEvent (B.VtyEvent ev)
handleAuthScreen _ = return ()
