{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import Client (Client)
import qualified Client
import Patch (Patch)
import qualified Patch
import Edit (Edit)
import qualified Edit
import Proto

import Data.IORef
import qualified Data.Vector as Vector
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import Control.Monad
import Control.Exception hiding (throw)
import Control.Concurrent
import qualified Network.WebSockets as WebSockets
import qualified Graphics.UI.Gtk as Gtk

main :: IO ()
main = WebSockets.runClient "localhost" 9000 "/ws" $ \connection -> do

  clientRef <- newIORef (Client.make "" 0)
  enableSignalsRef <- newIORef True

  void $ Gtk.initGUI

  window <- Gtk.windowNew
  void $ Gtk.on window Gtk.objectDestroy Gtk.mainQuit

  textView <- Gtk.textViewNew
  Gtk.set window [Gtk.containerChild Gtk.:= textView]

  textBuffer <- Gtk.textViewGetBuffer textView

  void $ Gtk.on textBuffer Gtk.bufferInsertText $ \iter txt -> do
    enableSignals <- readIORef enableSignalsRef
    when enableSignals $ do
      off <- Gtk.textIterGetOffset iter
      let patch = Patch.singleton edit
          edit = Edit.Insert off txt
      modifyIORef clientRef $ \client ->
        Client.applyLocal client patch
      checkClient clientRef connection

  void $ Gtk.on textBuffer Gtk.deleteRange $ \iter1 iter2 -> do
    enableSignals <- readIORef enableSignalsRef
    when enableSignals $ do
      off1 <- Gtk.textIterGetOffset iter1
      off2 <- Gtk.textIterGetOffset iter2
      let patch = Patch.singleton edit
          edit = Edit.Delete off1 (off2 - off1)
      modifyIORef clientRef $ \client ->
        Client.applyLocal client patch
      checkClient clientRef connection

  Gtk.widgetShowAll window

  void $ forkIO $ readLoop connection clientRef enableSignalsRef textBuffer

  Gtk.mainGUI

readLoop
  :: WebSockets.Connection
  -> IORef Client -> IORef Bool
  -> Gtk.TextBuffer
  -> IO ()
readLoop connection clientRef enableSignalsRef buffer = forever $ do
  msg <- do
    msg <- WebSockets.receiveDataMessage connection
    return $ case msg of
      WebSockets.Binary bs -> bs
      WebSockets.Text bs -> bs
  let either_patch = Aeson.eitherDecode msg >>= Aeson.parseEither patchParser
  case either_patch of
    Left err -> throwIO (ErrorCall err)
    Right patch -> Gtk.postGUISync $ do
      client <- readIORef clientRef
      let (client', patch') = Client.applyRemote client patch
      writeIORef clientRef client'

      writeIORef enableSignalsRef False
      applyPatch buffer patch'
      writeIORef enableSignalsRef True

      checkClient clientRef connection
  where

applyPatch :: Gtk.TextBuffer -> Patch -> IO ()
applyPatch buffer patch =
  Vector.mapM_ (applyEdit buffer) (Patch.edits patch)

applyEdit :: Gtk.TextBuffer -> Edit -> IO ()

applyEdit buffer (Edit.Insert at txt) = do
  iter <- Gtk.textBufferGetIterAtOffset buffer at
  Gtk.textBufferInsert buffer iter txt

applyEdit buffer (Edit.Delete from count) = do
  iter1 <- Gtk.textBufferGetIterAtOffset buffer from
  iter2 <- Gtk.textBufferGetIterAtOffset buffer (from + count)
  Gtk.textBufferDelete buffer iter1 iter2

checkClient :: IORef Client -> WebSockets.Connection -> IO ()
checkClient ref connection = do
  client <- readIORef ref
  case Client.checkSend client of
    Nothing -> return ()
    Just (client', patch, revision) -> do
      sendPatch connection patch revision
      writeIORef ref client'

sendPatch :: WebSockets.Connection -> Patch -> Int -> IO ()
sendPatch connection patch revision = do
  WebSockets.sendDataMessage connection (WebSockets.Binary bs)
  where
  bs = encodePatchAndRev patch revision
