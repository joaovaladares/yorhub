module Main (main) where

import qualified Brick as B

import Auth (initializeAuth)
import qualified Brick.BChan as B
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import qualified Graphics.Vty as V
import UI.App (app)
import UI.Screens.AuthScreen (CustomEvent (AnimationTick))

main :: IO ()
main = do
    chan <- B.newBChan 10
    void $ forkIO $ forever $ do
        B.writeBChan chan AnimationTick
        threadDelay 100000

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    st <- initializeAuth
    void $ B.customMain initialVty buildVty (Just chan) app st
