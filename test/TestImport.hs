{-# LANGUAGE QuasiQuotes #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Data.Aeson            (ToJSON, FromJSON, fromJSON, decode, Result (..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Network.Wai.Test      (SResponse (..))
import Test.Hspec            as X
import Test.Hspec            (Expectation, expectationFailure)
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: DB [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

assertJson :: (ToJSON a, FromJSON a)
    => (a -> Expectation)
    -> SResponse
    -> YesodExample App ()
assertJson assertion SResponse{simpleBody = lbs} = liftIO $
    case decode lbs of
        Nothing -> expectationFailure $ "Invalid JSON: "  ++ show (L8.unpack lbs)
        Just a ->
            case fromJSON a of
                Error s -> expectationFailure $ concat [s, "\nInput JSON: ", show a]
                Success x -> assertion x

expectResponseJson :: (ToJSON a, FromJSON a)
    => (a -> Expectation)
    -> YesodExample App ()
expectResponseJson assertion = withResponse (assertJson assertion)
