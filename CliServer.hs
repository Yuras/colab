{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import qualified Server
import Proto

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Foldable as Foldable
import Control.Monad
import Control.Exception hiding (throw)
import Control.Concurrent
import qualified Network.WebSockets as WebSockets
import System.Random

main :: IO ()
main = do
  serverVar <- newMVar (Server.make "" 0)
  clientsVar <- newMVar IntMap.empty
  uidVar <- newMVar 0

  WebSockets.runServer "0.0.0.0" 9000 $ \pending -> do
    connection <- WebSockets.acceptRequest pending
    withClient clientsVar uidVar connection $ forever $ do
      (patch, revision) <- do
        msg <- WebSockets.receiveDataMessage connection
        bs <- case msg of
          WebSockets.Binary bs -> return bs
          WebSockets.Text bs -> return bs
        let either_patch =
              Aeson.eitherDecode bs >>= Aeson.parseEither patchAndRevParser
        case either_patch of
          Left err -> throwIO (ErrorCall err)
          Right p -> return p

      n <- randomRIO (1, 10)
      threadDelay (n * 1000 * 1000)

      modifyMVar_ serverVar $ \server -> do
        let (server', patch') = Server.apply server patch revision
        clients <- readMVar clientsVar
        Foldable.forM_ clients $ \conn ->
          WebSockets.sendDataMessage conn
            . WebSockets.Binary
            . Aeson.encode
            . encodePatch
            $ patch'
        return server'

withClient
  :: MVar (IntMap WebSockets.Connection)
  -> MVar Int
  -> WebSockets.Connection
  -> IO a
  -> IO a
withClient clientsVar uidVar connection action = do
  uid <- newUID uidVar
  bracket_
    (addClient clientsVar uid connection)
    (removeClient clientsVar uid)
    action

newUID :: MVar Int -> IO Int
newUID var = modifyMVar var $ \i -> return (succ i, i)

addClient
  :: MVar (IntMap WebSockets.Connection)
  -> Int -> WebSockets.Connection
  -> IO ()
addClient var uid connection = modifyMVar_ var $ \clients ->
  return (IntMap.insert uid connection clients)

removeClient
  :: MVar (IntMap WebSockets.Connection)
  -> Int
  -> IO ()
removeClient var uid = modifyMVar_ var $ \clients ->
  return (IntMap.delete uid clients)
