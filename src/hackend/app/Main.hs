{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Core.Types
import Load (loadState)

main :: IO ()
main = do
    state <- initState "."
    scotty 3000 $
        -- get "/:word" $ do
        --     beam <- pathParam "word"
        --     html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
        get "/test" $ do
            json $ traits $ state

initState :: String -> IO State
initState = loadState