module GH.User (
    getUserName,
)
where

import qualified Data.Text as T
import GH.Error
import qualified GitHub as GH
import State (AuthError (..))

getUserName :: GH.Auth -> IO (Either AuthError T.Text)
getUserName auth = do
    res <- GH.github auth GH.userInfoCurrentR
    return $ either (Left . mapGHError) (Right . GH.untagName . GH.userLogin) res
