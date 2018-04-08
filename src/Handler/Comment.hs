module Handler.Comment where

import Import

getCommentR :: Handler Value
getCommentR = do
    comments :: [Entity Comment] <- runDB $ selectList [] []
    returnJson comments

postCommentR :: Handler Value
postCommentR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    comment <- requireJsonBody :: Handler Comment

    insertedComment <- runDB $ insertEntity comment
    returnJson insertedComment
