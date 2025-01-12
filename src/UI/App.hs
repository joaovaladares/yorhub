module UI.App (
    runApp,
) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Config (loadToken, saveToken)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GH.Auth (validateToken)
import Lens.Micro ((^.))
import State

drawMainMenu :: AppState -> B.Widget ()
drawMainMenu _ =
    B.vBox
        [ B.str "GitHub TUI - Main Menu"
        , B.str "-----------------------"
        , B.padTop (B.Pad 1) $
            B.vBox
                [ B.str "[L] Login with GitHub"
                , B.str "[D] View Demo Mode"
                , B.str "[Q] Exit"
                ]
        ]

drawLoginScreen :: AppState -> B.Widget ()
drawLoginScreen st =
    B.vBox
        [ B.str "GitHub Authentication"
        , B.str "----------------------"
        , B.padTop (B.Pad 1) $
            case st ^. authStatus of
                NotAuthenticated ->
                    B.vBox
                        [ B.str "Please enter your GitHub token:"
                        , B.hLimit 50 $
                            B.vLimit 10 $
                                B.border $
                                    E.renderEditor
                                        (B.str . T.unpack . T.concat)
                                        True
                                        (st ^. tokenInput)
                        ]
                Authenticating -> B.str "Authenticating..."
                AuthFailed err ->
                    B.vBox
                        [ B.withAttr errorAttr $
                            B.str $
                                "Authentication failed: " ++ show err
                        , B.str "Press Enter to try again"
                        ]
                Authenticated _ ->
                    B.withAttr successAttr $ B.str "Successfully authenticated!"
        , B.str ""
        , B.str "[Enter] Authenticate"
        , B.str "[Esc] Back to Main Menu"
        ]

drawDemoScreen :: AppState -> B.Widget ()
drawDemoScreen _ =
    B.vBox
        [ B.str "GitHub TUI - Demo Mode"
        , B.str "-----------------------"
        , B.padTop (B.Pad 1) $ B.str "This is a demo screen"
        , B.str ""
        , B.str "[Esc] Back to Main Menu"
        ]

drawUI :: AppState -> [B.Widget ()]
drawUI st =
    [ B.vBox
        [ case st ^. currentScreen of
            MainMenu -> drawMainMenu st
            LoginScreen -> drawLoginScreen st
            DemoScreen -> drawDemoScreen st
        ]
    ]

handleMainMenuEvent :: V.Event -> B.EventM () AppState ()
handleMainMenuEvent (V.EvKey (V.KChar 'l') []) =
    B.modify $ \s -> s{_currentScreen = LoginScreen}
handleMainMenuEvent (V.EvKey (V.KChar 'd') []) =
    B.modify $ \s -> s{_currentScreen = DemoScreen}
handleMainMenuEvent (V.EvKey (V.KChar 'q') []) = B.halt
handleMainMenuEvent _ = return ()

handleDemoScreenEvent :: V.Event -> B.EventM () AppState ()
handleDemoScreenEvent (V.EvKey V.KEsc []) =
    B.modify $ \s -> s{_currentScreen = MainMenu}
handleDemoScreenEvent _ = return ()

handleLoginScreenEvent :: V.Event -> B.EventM () AppState ()
handleLoginScreenEvent (V.EvKey V.KEsc []) =
    B.modify $ \s -> s{_currentScreen = MainMenu}
handleLoginScreenEvent (V.EvKey V.KEnter []) = do
    st <- B.get
    case st ^. authStatus of
        NotAuthenticated -> do
            let token = T.concat $ E.getEditContents $ st ^. tokenInput
            B.modify $ \s -> s{_authStatus = Authenticating}
            res <- liftIO $ validateToken token
            case res of
                Left err -> B.modify $ \s -> s{_authStatus = AuthFailed err}
                Right auth -> do
                    liftIO $ saveToken token
                    B.modify $ \s -> s{_authStatus = Authenticated auth}
        AuthFailed _ ->
            B.modify $ \s -> s{_authStatus = NotAuthenticated}
        _ -> return ()
handleLoginScreenEvent ev =
    B.zoom tokenInput $ E.handleEditorEvent (B.VtyEvent ev)

handleEvent :: B.BrickEvent () () -> B.EventM () AppState ()
handleEvent (B.VtyEvent ev) = do
    st <- B.get
    case st ^. currentScreen of
        MainMenu -> handleMainMenuEvent ev
        LoginScreen -> handleLoginScreenEvent ev
        DemoScreen -> handleDemoScreenEvent ev
handleEvent _ = return ()

errorAttr :: B.AttrName
errorAttr = B.attrName "error"

successAttr :: B.AttrName
successAttr = B.attrName "success"

app :: B.App AppState () ()
app =
    B.App
        { B.appDraw = drawUI
        , B.appChooseCursor = B.showFirstCursor
        , B.appHandleEvent = handleEvent
        , B.appStartEvent = return ()
        , B.appAttrMap =
            const $
                B.attrMap
                    V.defAttr
                    [ (errorAttr, V.withForeColor V.defAttr V.red)
                    , (successAttr, V.withForeColor V.defAttr V.green)
                    ]
        }

runApp :: IO ()
runApp = do
    -- Try to load existing token
    savedToken <- loadToken
    case savedToken of
        Just token -> do
            result <- validateToken token
            case result of
                Left _ ->
                    void $ B.defaultMain app initialState
                Right auth ->
                    void $ B.defaultMain app $ initialState{_authStatus = Authenticated auth}
        Nothing ->
            void $ B.defaultMain app initialState
