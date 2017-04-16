{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Lens ((^..), (^.), only, ix, toListOf, (^?), to, (&), (.~))
import Control.Monad
import Data.Aeson
import Data.Attoparsec.Text.Lazy hiding (take, try)
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding
import GHC.Generics hiding (to)
import Network.Wreq
import System.Environment
import Text.Taggy
import Text.Taggy.Lens

newtype Pkg = Pkg
  { packageName :: String
  } deriving (Generic, Show)

instance FromJSON Pkg
instance ToJSON Pkg

newtype Dep = Dep Text
  deriving (Show, Eq)

getPkgs :: IO [Pkg]
getPkgs = do
  let opts = defaults & header "Accept" .~ ["application/json"]
  r <- asJSON =<< getWith opts "https://hackage.haskell.org/packages"
  pure (r ^. responseBody)

getDeps (Pkg name) = do
  r <- get $ "https://hackage.haskell.org/package/" <> name <> "/dependencies"
  pure $ r ^. responseBody
           . to (decodeUtf8With lenientDecode)
           . html
           . allNamed (only "div")
           . attributed (ix "id" . only "detailed-dependencies")
           . allNamed (only "li")
           . allNamed (only "a")
           . children
           . to dep
   where
     dep = map $ \(NodeContent x) -> Dep x

hasUnix :: [Dep] -> Bool
hasUnix = any (== Dep "unix")

main :: IO ()
main = do
  mvar <- newMVar []
  pkgs <- getPkgs
  forM_ pkgs $ \pkg -> do
    result <- try (getDeps pkg >>= \deps -> 
      when (hasUnix deps) $ do
        modifyMVar_ mvar $ \xs -> pure (pkg : xs))
    case result of
      Right _ -> pure ()
      Left (e :: SomeException) -> print e
  vals <- readMVar mvar
  writeFile "log" (show vals)
