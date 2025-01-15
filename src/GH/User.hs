module GH.User (
    getUserName,
)
where

import qualified Data.Text as T
import qualified GitHub as GH
import State (AuthError (..))

getUserName :: GH.Auth -> IO (Either AuthError T.Text)
getUserName auth = do
    res <- GH.github auth GH.userInfoCurrentR
    case res of
        Left (GH.JsonError _) -> return $ Left InvalidToken
        Left (GH.HTTPError e) -> return $ Left $ NetworkError (show e)
        Left e -> return $ Left $ UnknownError (show e)
        Right user -> return $ Right $ GH.untagName $ GH.userLogin user
