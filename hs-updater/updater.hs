#!/usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -p nix
#! nix-shell -p "haskellPackages.ghcWithPackages (self: with self; [ aeson-pretty microlens-aeson req ])"

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Data.Default.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Text (Text, intercalate, pack, unpack)
import GHC.Generics
import Lens.Micro
import Lens.Micro.Aeson
import Network.HTTP.Req
import Prelude hiding (readFile, writeFile)
import System.Environment (getArgs)
import System.Process

data Project = Project { owner, repo, rev, sha256 :: Text }
    deriving (Generic, FromJSON, ToJSON)

r :: MonadHttp m => Project -> Text -> m (JsonResponse Value)
r p b = req GET
    (https "api.github.com" /: "repos" /: owner p /: repo p /: "branches" /: b)
    NoReqBody jsonResponse (header "User-Agent" "vaibhavsagar")

getRev :: Project -> Text -> MaybeT IO Text
getRev project branch = MaybeT $ do
    res <- responseBody <$> runReq def (r project branch)
    pure $ res ^? key "commit" . key "sha" . _String

buildURL :: Project -> Text
buildURL prj = "https://github.com/" <> path <> ".tar.gz"
    where path = intercalate "/" [owner prj, repo prj, "archive", rev prj]

getSha256 :: Text -> Bool -> IO Text
getSha256 url doUnpack = pack . init <$>
    readProcess "nix-prefetch-url" (["--unpack" | doUnpack ] ++ [unpack url]) ""

modify :: FilePath -> Text -> Text -> Bool -> MaybeT IO Value
modify fName pName bName doUnpack = do
    versions <- MaybeT $ decode <$> readFile fName
    project <- MaybeT . pure $ parseMaybe parseJSON =<< versions ^? key pName
    rev' <- getRev project bName
    sha256' <- lift $ getSha256 (buildURL project { rev = rev' }) doUnpack
    let project' = project { rev = rev', sha256 = sha256' }
    pure $ versions & key pName .~ toJSON project'

update :: FilePath -> Text -> Text -> Bool -> IO ()
update fName pName bName doUnpack =
    runMaybeT (modify fName pName bName doUnpack) >>= maybe
        (pure ()) (writeFile fName . encodePretty' defConfig
            { confIndent = Spaces 2, confCompare = compare })

main :: IO ()
main = getArgs >>= \case
    a | length a < 2 -> putStrLn "Not enough arguments!"
    [fName, pName] -> update fName (pack pName) "master" True
    [fName, pName, bName] -> update fName (pack pName) (pack bName) True
    _ -> putStrLn "Too many arguments!"
