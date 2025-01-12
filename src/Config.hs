module Config (
    Config (..),
    saveToken,
    loadToken,
) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified System.Directory as Dir
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
    configPath <- getConfigPath
    exists <- Dir.doesFileExist configPath
    if exists
        then Just . decodeUtf8 <$> BS.readFile configPath
        else return Nothing
