module Solution where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = do
    btn <- UI.button # set UI.text "Click Me!"
    getBody window #+ [element btn]

    on UI.click btn $ \() ->
        element btn # set UI.text "Thanks!"
