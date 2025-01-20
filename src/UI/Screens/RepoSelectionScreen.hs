{-# LANGUAGE OverloadedStrings #-}

module UI.Screens.RepoSelectionScreen (
    drawRepoSelectionScreen,
    handleRepoSelectionScreen,
) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Control.Monad.IO.Class (MonadIO (liftIO))
import GH.Repo (RepoError (InvalidRepoFormat, RepoAccessError), getRepository)
import Lens.Micro ((^.))
import State
import UI.Types
import UI.Widgets.CommandBar (drawCommandBar)

drawRepoSelectionScreen :: AppState -> B.Widget Name
drawRepoSelectionScreen st =
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

handleRepoSelectionScreen :: V.Event -> B.EventM Name AppState ()
handleRepoSelectionScreen (V.EvKey V.KEsc []) = B.halt
handleRepoSelectionScreen (V.EvKey V.KEnter []) = do
    st <- B.get
    let repoPath = T.concat $ E.getEditContents $ st ^. repoInput
    case T.splitOn "/" repoPath of
        [owner, repo] -> do
            B.modify $ \s -> s{_repoStatus = Loading}
            case st ^. authStatus of
                Authenticated auth -> do
                    res <- liftIO $ getRepository auth owner repo
                    case res of
                        Left err ->
                            B.modify $ \s -> s{_repoStatus = LoadFailed (RepoAccessError (show err))}
                        Right repoInfo -> do
                            B.modify $ \s ->
                                s
                                    { _repoStatus = Loaded repoInfo
                                    , _currentScreen = RepoDetailScreen
                                    }
                _ -> return ()
        _ -> B.modify $ \s -> s{_repoStatus = LoadFailed InvalidRepoFormat}
handleRepoSelectionScreen ev =
    B.zoom repoInput $ E.handleEditorEvent (B.VtyEvent ev)
