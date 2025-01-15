{-# LANGUAGE OverloadedStrings #-}

module UI.App (
    runApp,
)
where

import Brick (zoom)
import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as E
import Config (loadToken, saveToken)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import GH.Auth (validateToken)
import GH.User (getUserName)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import State

drawCommandBar :: AppState -> B.Widget ()
drawCommandBar st =
    B.vLimit 1 $
        B.withAttr commandBarAttr $
            B.hBox $
                case st ^. currentScreen of
                    AuthScreen ->
                        [ B.str "ENTER"
                        , B.str " Authenticate "
                        , B.str "│ "
                        , B.str "ESC"
                        , B.str " Quit"
                        ]
                    RepoScreen ->
                        [ B.str "ENTER"
                        , B.str " Open Repo "
                        , B.str "│ "
                        , B.str "q"
                        , B.str " Quit"
                        ]

drawAuthScreen :: AppState -> B.Widget ()
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

drawRepoScreen :: AppState -> B.Widget ()
drawRepoScreen st =
    B.vBox
        [ B.vLimit 1 $
            B.hBox
                [ B.str "YorHub"
                , B.padLeft B.Max $ B.str $ "Welcome, " ++ maybe "User" T.unpack (st ^. username)
                ]
        , B.hBorder
        , B.padAll 1 $
            B.vBox
                [ B.str "Enter a repository to explore (format: owner/repo):"
                , B.padTop (B.Pad 1) $
                    B.hLimit 50 $
                        B.vLimit 10 $
                            B.withAttr inputAttr $
                                B.border $
                                    E.renderEditor (B.str . T.unpack . T.concat) True (st ^. repoInput)
                                    -- Could add recent repos here later
                ]
        , B.fill ' '
        , B.hBorder
        , drawCommandBar st
        ]

drawUI :: AppState -> [B.Widget ()]
drawUI st =
    [ B.vBox
        [ case st ^. currentScreen of
            AuthScreen -> drawAuthScreen st
            RepoScreen -> drawRepoScreen st
        ]
    ]

handleAuthScreen :: V.Event -> B.EventM () AppState ()
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
                                    , _currentScreen = RepoScreen
                                    , _username = Just name
                                    }
                Left err -> B.modify $ \s -> s{_authStatus = AuthFailed err}
        AuthFailed _ ->
            B.modify $ \s -> s{_authStatus = NotAuthenticated}
        _ -> return ()
handleAuthScreen ev = zoom tokenInput $ E.handleEditorEvent (B.VtyEvent ev)

handleRepoScreen :: V.Event -> B.EventM () AppState ()
handleRepoScreen (V.EvKey (V.KChar 'q') []) = B.halt
handleRepoScreen (V.EvKey V.KEnter []) = do
    st <- B.get
    let repoPath = T.concat $ E.getEditContents $ st ^. repoInput
    case T.splitOn "/" repoPath of
        [owner, repo] -> do
            -- Validate and open repository view
            return ()
        _ -> return () -- TODO: Show error for invalid format
handleRepoScreen ev =
    zoom repoInput $ E.handleEditorEvent (B.VtyEvent ev)

handleEvent :: B.BrickEvent () () -> B.EventM () AppState ()
handleEvent (B.VtyEvent ev) = do
    st <- B.get
    case st ^. currentScreen of
        AuthScreen -> handleAuthScreen ev
        RepoScreen -> handleRepoScreen ev
handleEvent _ = return ()

inputAttr :: B.AttrName
inputAttr = B.attrName "input"

commandBarAttr :: B.AttrName
commandBarAttr = B.attrName "commandBar"

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
                    , (inputAttr, V.withStyle V.defAttr V.underline)
                    , (commandBarAttr, V.withStyle (V.withForeColor V.defAttr V.brightBlack) V.bold)
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
                Left _ -> do
                    void $ B.defaultMain app initialState
                Right auth -> do
                    userRes <- getUserName auth
                    let initialUser = either (const Nothing) Just userRes
                    void $
                        B.defaultMain app $
                            initialState
                                { _authStatus = Authenticated auth
                                , _currentScreen = RepoScreen
                                , _username = initialUser
                                }
        Nothing ->
            void $ B.defaultMain app initialState
