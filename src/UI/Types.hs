module UI.Types (
    Name (..),
    inputAttr,
    commandBarAttr,
    errorAttr,
    successAttr,
    asciiArtAttr,
    yorhaAttr,
    titleAttr,
    statusAttr,
    commandAttr,
    systemAttr,
    uiAttributeMap,
) where

import qualified Brick as B
import qualified Graphics.Vty as V

data Name
    = TokenEditor
    | RepoEditor
    deriving (Eq, Ord, Show)

-- Attributes
inputAttr, commandBarAttr, errorAttr, successAttr, asciiArtAttr, yorhaAttr, titleAttr, statusAttr, commandAttr, systemAttr :: B.AttrName
inputAttr = B.attrName "input"
commandBarAttr = B.attrName "commandBar"
errorAttr = B.attrName "error"
successAttr = B.attrName "success"
asciiArtAttr = B.attrName "asciiArt"
yorhaAttr = B.attrName "yorha"
titleAttr = B.attrName "title"
statusAttr = B.attrName "status"
commandAttr = B.attrName "command"
systemAttr = B.attrName "system"

uiAttributeMap :: B.AttrMap
uiAttributeMap =
    B.attrMap
        V.defAttr
        [ (errorAttr, V.withForeColor V.defAttr V.red)
        , (successAttr, V.withForeColor V.defAttr V.green)
        , (inputAttr, V.withStyle V.defAttr V.underline)
        , (commandBarAttr, V.withStyle (V.withForeColor V.defAttr V.brightBlack) V.bold)
        , (asciiArtAttr, V.withStyle (V.withForeColor V.defAttr V.brightBlack) V.bold)
        , (yorhaAttr, V.withForeColor V.defAttr V.brightCyan)
        , (titleAttr, V.withStyle (V.withForeColor V.defAttr V.brightBlack) V.bold)
        , (systemAttr, V.withForeColor V.defAttr V.brightGreen)
        , (statusAttr, V.withForeColor V.defAttr V.brightGreen)
        , (commandAttr, V.withStyle (V.withForeColor V.defAttr V.brightBlack) V.bold)
        ]
