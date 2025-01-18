module UI.App (
    app,
) where

import qualified Brick as B

import Lens.Micro ((^.))
import State
import UI.Screens.AuthScreen
import UI.Screens.RepoSelectionScreen
import UI.Types (Name, uiAttributeMap)

app :: B.App AppState () Name
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

handleEvent :: B.BrickEvent Name () -> B.EventM Name AppState ()
handleEvent (B.VtyEvent ev) = do
    st <- B.get
    case st ^. currentScreen of
        AuthScreen -> handleAuthScreen ev
        RepoSelectionScreen -> handleRepoSelectionScreen ev
handleEvent _ = return ()
