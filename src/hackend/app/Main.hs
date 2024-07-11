{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Core.Types
import Load (loadState)

main :: IO ()
main = do
    state <- initState "./lessons/"
    scotty 3000 $
        -- get "/:word" $ do
        --     beam <- pathParam "word"
        --     html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
        get "/test" $ do
            json $ state

initState :: String -> IO State
initState = loadState