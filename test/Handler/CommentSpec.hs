{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.CommentSpec (spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do
    describe "valid requests" $ do
        it "creates a new comment" $ do
            let message = "My message" :: Text
                body = object [ "message" .= message ]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200

            [Entity _id comment] <- runDB $ selectList [CommentMessage ==. message] []
            assertEq "Should have " comment (Comment message)

        it "lists existing comments" $ do
            let comments = [Comment "Hello world!", Comment "Another one..."]

            commentEntities <- forM comments (runDB . insertEntity)
            
            request $ do
                setMethod "GET"
                setUrl CommentR

            statusIs 200

            expectResponseJson (`shouldMatchList` commentEntities)

    describe "invalid requests" $ do
        it "400s when the JSON body is invalid" $ do
            let body = object [ "foo" .= ("My message" :: Value) ]

            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400

