module UI.Screens.RepoDetailScreen (
    drawRepoDetailScreen,
    handleRepoDetailScreen,
) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import GH.Repo (RepoInfo (..))
import Lens.Micro ((^.))
import State
import UI.Types
import UI.Widgets.CommandBar (drawCommandBar)

drawRepoDetailScreen :: AppState -> B.Widget Name
drawRepoDetailScreen st =
    case st ^. repoStatus of
        Loaded repo ->
            B.vBox
                [ B.vLimit 1 $ B.str $ T.unpack (repoOwner repo) <> "/" <> T.unpack (repoName repo)
                , B.hBorder
                , B.padAll 1 $
                    B.vBox
                        [ B.str "Description: " B.<+> maybe (B.str "No description") (B.str . T.unpack) (repoDescription repo)
                        , B.str $ "Stars: " <> show (repoStars repo)
                        -- Add more repository information here
                        ]
                , B.fill ' '
                , B.hBorder
                , drawCommandBar st
                ]
        _ -> B.str "Loading repository..."

handleRepoDetailScreen :: V.Event -> B.EventM Name AppState ()
handleRepoDetailScreen (V.EvKey (V.KChar 'q') []) =
    B.modify $ \s -> s{_currentScreen = RepoSelectionScreen}
handleRepoDetailScreen _ = return ()
