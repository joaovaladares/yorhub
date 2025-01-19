module Config (
    Config (..),
    saveToken,
    loadToken,
) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified System.Directory as Dir
import System.Directory.Internal.Prelude (hPutStrLn, lookupEnv, stderr)
import System.FilePath (takeDirectory, (</>))

newtype Config = Config
    { authToken :: Maybe T.Text
    }
    deriving (Show)

getConfigPath :: IO FilePath
getConfigPath = do
    home <- Dir.getHomeDirectory
    return $ home </> ".config" </> "yorhub" </> "config"

saveToken :: T.Text -> IO ()
saveToken token = do
    configPath <- getConfigPath
    Dir.createDirectoryIfMissing True (takeDirectory configPath)
    BS.writeFile configPath (encodeUtf8 token)

loadToken :: IO (Maybe T.Text)
loadToken = do
    -- Allow the user to override the token with an environment variable
    envToken <- fmap T.pack <$> lookupEnv "GITHUB_TOKEN"
    case envToken of
        Just token -> return $ Just token
        Nothing -> do
            configPath <- getConfigPath
            exists <- Dir.doesFileExist configPath
            if exists
                then do
                    hPutStrLn stderr "Loading token from config file..."
                    Just . decodeUtf8 <$> BS.readFile configPath
                else return Nothing
