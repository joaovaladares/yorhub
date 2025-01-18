module UI.Types (
    Name (..),
    inputAttr,
    commandBarAttr,
    errorAttr,
    successAttr,
    uiAttributeMap,
) where

import qualified Brick as B
import qualified Graphics.Vty as V

data Name
    = TokenEditor
    | RepoEditor
    deriving (Eq, Ord, Show)

-- Attributes
inputAttr, commandBarAttr, errorAttr, successAttr :: B.AttrName
inputAttr = B.attrName "input"
commandBarAttr = B.attrName "commandBar"
errorAttr = B.attrName "error"
successAttr = B.attrName "success"

uiAttributeMap :: B.AttrMap
uiAttributeMap =
    B.attrMap
        V.defAttr
        [ (errorAttr, V.withForeColor V.defAttr V.red)
        , (successAttr, V.withForeColor V.defAttr V.green)
        , (inputAttr, V.withStyle V.defAttr V.underline)
        , (commandBarAttr, V.withStyle (V.withForeColor V.defAttr V.brightBlack) V.bold)
        ]
