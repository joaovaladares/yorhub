module Main (main) where

import qualified Brick as B

import Auth (initializeAuth)
import Control.Monad (void)
import UI.App (app)

main :: IO ()
main = do
    st <- initializeAuth
    void $ B.defaultMain app st
