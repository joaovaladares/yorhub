module GH.Auth (
    validateToken,
    loadAndValidateToken,
)
where

import Config (loadToken)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GH.Error (mapGHError)
import qualified GitHub as GH
import State (AuthError (..))

validateToken :: T.Text -> IO (Either AuthError GH.Auth)
validateToken token = do
    let auth = GH.OAuth $ T.encodeUtf8 token
    res <- GH.github auth GH.userInfoCurrentR
    return $ either (Left . mapGHError) (const $ Right auth) res

loadAndValidateToken :: IO (Either AuthError GH.Auth)
loadAndValidateToken = do
    token <- loadToken
    case token of
        Nothing -> return $ Left InvalidToken
        Just t -> validateToken t
