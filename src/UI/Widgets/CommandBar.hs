module UI.Widgets.CommandBar (
    drawCommandBar,
) where

import qualified Brick as B
import Lens.Micro ((^.))
import State (AppState, Screen (..), currentScreen)
import UI.Types (Name, commandBarAttr)

drawCommandBar :: AppState -> B.Widget Name
drawCommandBar st =
    B.vLimit 1 $
        B.withAttr commandBarAttr $
            case st ^. currentScreen of
                AuthScreen ->
                    B.str "ENTER: Authenticate │ ESC: Quit"
                RepoSelectionScreen ->
                    B.str "ENTER: Open Repo │ ESC: Quit"
