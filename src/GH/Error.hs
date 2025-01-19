module GH.Error (
    mapGHError,
) where

import qualified GitHub as GH
import State (AuthError (..))

mapGHError :: GH.Error -> AuthError
mapGHError (GH.JsonError _) = InvalidToken
mapGHError (GH.HTTPError e) = NetworkError (show e)
mapGHError e = UnknownError (show e)
