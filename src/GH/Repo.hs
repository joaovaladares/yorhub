module GH.Repo (
    RepoInfo (..),
    RepoError (..),
    getRepository,
) where

import qualified Data.Text as T
import qualified GitHub as GH

data RepoInfo = RepoInfo
    { repoName :: T.Text
    , repoOwner :: T.Text
    , repoDescription :: Maybe T.Text
    , repoStars :: Int
    }
    deriving (Show, Eq)

data RepoError
    = InvalidRepoFormat
    | RepoNotFound
    | RepoAccessError String
    deriving (Show, Eq)

getRepository :: GH.Auth -> T.Text -> T.Text -> IO (Either RepoError RepoInfo)
getRepository auth owner repo = do
    let owner' = GH.mkOwnerName owner
        repo' = GH.mkRepoName repo
    res <- GH.github auth $ GH.repositoryR owner' repo'
    case res of
        Left err -> pure $ Left $ RepoAccessError (show err)
        Right repoInfo ->
            pure $
                Right $
                    RepoInfo
                        { repoName = GH.untagName $ GH.repoName repoInfo
                        , repoOwner = GH.untagName $ GH.simpleOwnerLogin $ GH.repoOwner repoInfo
                        , repoDescription = GH.repoDescription repoInfo
                        , repoStars = GH.repoStargazersCount repoInfo
                        }
