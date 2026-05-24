{-# LANGUAGE OverloadedStrings #-}

module Vivid.Tidal (test, mainTest, repeatTest, tidalSend, playProxy, setupVivid, vividProxy, vividReload, vividSave, vividPush) where

import Vivid
import Vivid.OSC as O
import Sound.Tidal.Context hiding (wait, lag, saw)

import Network.Socket
import Network.Socket.ByteString as SB

import Control.Concurrent.STM (atomically, writeTVar)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Bits ((.|.), shiftL)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

-- | Saves a SynthDef to disk without notifying SuperCollider
vividSave :: String -> SynthDef a -> IO ()
vividSave name sdDef = do
   let path = "/home/numa/.local/share/SuperCollider/synthdefs/" ++ name ++ ".scsyndef"
   putStrLn $ "Saving " ++ name ++ " to " ++ path
   BS.writeFile path (encodeSD sdDef)

-- | Notifies SuperCollider to reload all SynthDefs from its synthdefs directory
vividReload :: IO ()
vividReload = do
   _ <- tidalSend $ O.OSC "/vivid/reload" []
   return ()

-- | Sends to server memory AND saves to disk for SuperDirt
vividPush :: String -> SynthDef a -> IO ()
vividPush name sdDef = do
   defineSD sdDef
   vividSave name sdDef

setupVivid :: IO ()
setupVivid = do
   putStrLn "Cleaning Vivid global state and SuperCollider..."
   
   -- 1. Disconnect if already connected
   closeSCServerConnection
   
   -- 2. Hard reset of the global state allocators
   -- This prevents "Node ID already in use" errors after restarting
   withGlobalSCServerState $ \st -> atomically $ do
      -- Reset Node IDs (starting from a safe range)
      writeTVar (_scServerState_availableNodeIds st) $ 
         map (NodeId . ((1 `shiftL` 26) .|.)) [1000..]
      -- Reset Buffer IDs
      writeTVar (_scServerState_availableBufferIds st) $ 
         drop 512 $ map BufferId [0..]
      -- Reset Sync IDs
      writeTVar (_scServerState_availableSyncIds st) $ 
         drop 10000 $ map SyncId [0..]
      -- Clear defined SynthDefs set so they get re-sent if needed
      writeTVar (_scServerState_definedSDs st) Set.empty
      -- Clear sync mailboxes
      writeTVar (_scServerState_syncIdMailboxes st) Map.empty
      -- Reset connection started flag
      writeTVar (_scServerState_socketConnectStarted st) False

   -- 3. Connect to server
   putStrLn "Connecting to SuperCollider (scsynth) on 127.0.0.1:57110..."
   res <- createSCServerConnection defaultConnectConfig
   case res of
      Right _ -> do
         putStrLn "Connected to SuperCollider!"
         putStrLn "Server is ready. (Note: Run 'SynthDescLib.global.read' in SC if Tidal can't find your synths)"
      Left err -> do
         putStrLn $ "Could not connect: " ++ err
         putStrLn "Make sure scsynth is running: scsynth -u 57110"

-- :set -package vivid
-- :set -package vivid-osc
-- :set -XOverloadedStrings
-- :set -XDataKinds

-- need to
-- (s.reboot {s.options.numBuffers = 1024 * 1024;s.options.memSize = 8192 * 128;s.waitForBoot{~dirt.stop;~dirt = SuperDirt(2,s);~dirt.loadSoundFiles;s.sync;~dirt.start(57120, 0 ! 12);} s.latency = 0.6;};)
-- from sclang, to load SuperDirt

test = do
   (a:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "57120")
   s <- socket (addrFamily a) Datagram defaultProtocol
   connect s (addrAddress a)
   SB.send s $ encodeOSC $
      O.OSC "/play2" [OSC_S "cps", OSC_F 1.2, OSC_S "s", OSC_S "bd"]

osc_msg1 = O.OSC "/play2" [OSC_S "cps", OSC_F 1.2, OSC_S "s", OSC_S "bd"]
osc_msg2 = O.OSC "/play2" [OSC_S "cps", OSC_F 1.2, OSC_S "s", OSC_S "sd"]
osc_msg3 = O.OSC "/play2" [OSC_S "cps", OSC_F 1.2, OSC_S "s", OSC_S "cp"]

tidalSend osc_mesg = do
   (a:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "57120")
   s <- socket (addrFamily a) Datagram defaultProtocol
   connect s (addrAddress a)
   SB.send s $ encodeOSC $ osc_mesg

mainTest :: IO()
mainTest =
  do
    tidalSend osc_msg1
    wait 1
    tidalSend osc_msg2
    wait 0.5
    tidalSend osc_msg3
    wait 0.25
    tidalSend osc_msg3
    wait 0.25
    tidalSend osc_msg1
    wait 0.5
    tidalSend osc_msg3
    wait 0.5
    tidalSend osc_msg2
    wait 0.5
    tidalSend osc_msg3
    wait 0.25
    tidalSend osc_msg3
    wait 0.25

repeatTest :: Int -> IO()
repeatTest 0 = pure ()
repeatTest n = mainTest >> repeatTest (n -1)

-- SuperDirt compatible SynthDef
vividProxy = sdNamed "vividProxy" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq") $ do
   -- We use line as a simple envelope, scaled by sustain
   -- doneAction_ (2::Float) is crucial so SuperDirt knows when the synth is finished
   env <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
   
   sig <- saw (freq_ (V::V "freq"))
   
   -- Replicating SuperDirt's DirtPan logic for stereo:
   -- 1. Multiply signal by envelope
   -- 2. Pan using pan2, mapping Tidal's [0,1] to SC's [-1,1]
   s <- pan2 (in_ (sig ~* env), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

playProxy :: IO ()
playProxy = do
    -- For local testing we provide default values
    x <- synth vividProxy (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq")
    wait (1 :: Float)
    freeAll

-- freeAll

-- putStrLn "Playing vividProxy synth with OSC control"

-- putStrLn "Tidal is ready to use! Type :t d1 to see its type, and :t
-- p to  see the  type of the  function used to  send patterns  to the
-- SuperDirt synth. Happy coding!"
    
--- Vivid fragment end

{-
tone = sd (0 ::I "note") $ do
       a <- lfTri (freq_ 0.2) ? KR ~* 0.5 ~+ 0.5
       freq <- lag (in_ $ midiCPS (V::V "note"), lagSecs_ 1.25) ? KR
       b <- 0.03 ~* varSaw (freq_ freq, width_ a)
       out 0 [b, b]
-}

-- x <- synth tone ()

-- freeAll

-- 2 + 2

-- out (bus_ (V ::I "out"), signal_ (s2 ~* env))
