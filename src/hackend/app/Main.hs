{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Core.Types

main :: IO ()
main = scotty 3000 $
    get "/:word" $ do
        beam <- pathParam "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

load :: IO Topic
load = undefined