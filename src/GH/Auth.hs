module GH.Auth (
    validateToken,
) where

import qualified Data.Text as T
import qualified GitHub as GH

import qualified Data.Text.Encoding as T
import State (AuthError (..))

validateToken :: T.Text -> IO (Either AuthError GH.Auth)
validateToken token = do
    let auth = GH.OAuth $ T.encodeUtf8 token
    res <- GH.github auth GH.userInfoCurrentR
    case res of
        Left (GH.JsonError _) -> return $ Left InvalidToken
        Left (GH.HTTPError e) -> return $ Left $ NetworkError (show e)
        Left e -> return $ Left $ UnknownError (show e)
        Right _ -> return $ Right auth
