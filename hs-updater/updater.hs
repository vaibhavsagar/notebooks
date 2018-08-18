#!/usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -p nix
#! nix-shell -p "haskellPackages.ghcWithPackages (self: with self; [ aeson-pretty microlens-aeson req ])"

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Data.Default.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Lens.Micro
import Lens.Micro.Aeson
import Network.HTTP.Req
import Prelude hiding (readFile, writeFile)
import System.Environment (getArgs)
import System.Process

data Project = Project
    { owner  :: Text
    , repo   :: Text
    , rev    :: Text
    , sha256 :: Text
    } deriving (Show, Generic)

instance FromJSON Project
instance ToJSON Project

extractProject :: Value -> Text -> Maybe Project
extractProject versions name = fromJSON <$> versions ^? key name >>= \case
    Error _ -> Nothing
    Success p -> Just p

r :: (MonadHttp m, FromJSON a) => Project -> Text -> m (JsonResponse a)
r project branch = req GET
    (  https "api.github.com"
    /: "repos"
    /: owner project
    /: repo project
    /: "branches"
    /: branch
    ) NoReqBody jsonResponse (header "User-Agent" "vaibhavsagar")

getRev :: Project -> Text -> MaybeT IO Text
getRev project branch = do
    res <- lift (responseBody <$> runReq def (r project branch) :: IO Value)
    MaybeT . pure $ res ^? key "commit" . key "sha" . _String

buildURL :: Project -> Text
buildURL project
    =  "https://github.com/"
    <> owner project <> "/"
    <> repo project  <> "/"
    <> "archive"     <> "/"
    <> rev project   <> ".tar.gz"

getSha256 :: Text -> Bool -> IO Text
getSha256 url doUnpack = let
    option = ["--unpack" | doUnpack]
    in pack . init <$>
        readProcess "nix-prefetch-url" (option ++ [unpack url]) ""

modify :: FilePath -> Text -> Text -> Bool -> MaybeT IO Value
modify filename projectName branchName doUnpack = do
    versions <- MaybeT (decode <$> readFile filename :: IO (Maybe Value))
    project <- MaybeT . pure $ extractProject versions projectName
    rev' <- getRev project branchName
    sha256' <- lift $ getSha256 (buildURL project { rev = rev' }) doUnpack
    let project' = project { rev = rev', sha256 = sha256' }
    pure $ versions & key projectName .~ toJSON project'

update :: FilePath -> Text -> Text -> Bool -> IO ()
update filename projectName branchName doUnpack =
    runMaybeT (modify filename projectName branchName doUnpack) >>= \case
        Just updated -> writeFile filename (encodePretty'
            defConfig { confIndent = Spaces 2, confCompare = compare } updated)
        Nothing -> pure ()

main :: IO ()
main = getArgs >>= \case
    a | length a < 2 -> putStrLn "Not enough arguments!"
    [filename, projectName] ->
        update filename (pack projectName) "master" True
    [filename, projectName, branchName] ->
        update filename (pack projectName) (pack branchName) True
    _ -> putStrLn "Too many arguments!"
