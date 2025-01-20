module UI.App (
    app,
) where

import qualified Brick as B

import Lens.Micro ((^.))
import State
import UI.Screens.AuthScreen
import UI.Screens.RepoDetailScreen
import UI.Screens.RepoSelectionScreen
import UI.Types (Name, uiAttributeMap)

app :: B.App AppState CustomEvent Name
app =
    B.App
        { B.appDraw = drawUI
        , B.appChooseCursor = B.showFirstCursor
        , B.appHandleEvent = handleEvent
        , B.appStartEvent = return ()
        , B.appAttrMap = const uiAttributeMap
        }

drawUI :: AppState -> [B.Widget Name]
drawUI st =
    case st ^. currentScreen of
        AuthScreen -> [drawAuthScreen st]
        RepoSelectionScreen -> [drawRepoSelectionScreen st]
        RepoDetailScreen -> [drawRepoDetailScreen st]

handleEvent :: B.BrickEvent Name CustomEvent -> B.EventM Name AppState ()
handleEvent e@(B.VtyEvent ev) = do
    st <- B.get
    case st ^. currentScreen of
        AuthScreen -> handleAuthScreen e
        RepoSelectionScreen -> handleRepoSelectionScreen ev
        RepoDetailScreen -> handleRepoDetailScreen ev
handleEvent e@(B.AppEvent AnimationTick) = do
    st <- B.get
    case st ^. currentScreen of
        AuthScreen -> handleAuthScreen e
        _ -> return ()
handleEvent _ = return ()
