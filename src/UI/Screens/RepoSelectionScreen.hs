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
        [_, _] -> do
            -- Validate and open repository view
            return ()
        _ -> return () -- TODO: Show error for invalid format
handleRepoSelectionScreen ev =
    B.zoom repoInput $ E.handleEditorEvent (B.VtyEvent ev)
