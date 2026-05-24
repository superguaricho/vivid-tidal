{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Vivid.Tidal.Cymbals where

import Vivid
import Vivid.Tidal

-- | Función para cargar los synths en el servidor y registrarlos para Tidal
loadCymbals :: IO ()
loadCymbals = do
    putStrLn "Cargando platillos Vivid..."
    vividPush "vividCym" vividCymbalGraph
    vividPush "vividCymS" vividCymbalWithStick
    vividPush "vividCymSusp" vividSuspendedCym
    vividPush "vividCymSplash" vividSplashCym
    vividPush "vividRide" vividRide
    vividReload
    putStrLn "¡Platillos Vivid listos para Tidal!"

-- | Frecuencias exponenciales distribuidas (100 resonadores)
expFreqs :: [Float]
expFreqs = [300 * (20000/300)**(i/99) | i <- [0..99]]

-- | Frecuencias para Ride (más altas, desde 1kHz)
rideFreqs :: [Float]
rideFreqs = [1000 * (20000/1000)**(i/99) | i <- [0..99]]

-- | vividRide: Bright Jazz Ride
-- Focused on "ping" definition and high-frequency shimmer.
vividRide = sdNamed "vividRide" (0::I "out", 5::I "sustain", 0::I "pan") $ do
   let dur = V::V "sustain"
       p = V::V "pan"
   
   -- Stick "Ping": very short, very high frequency
   stickEnv <- line (start_ (1::Float), end_ (0::Float), duration_ (0.005::Float))
   n1 <- whiteNoise
   let stick = hpf (in_ (n1 ~* stickEnv ~* (0.4::Float)), freq_ (10000::Float))
   
   -- Shimmer excitation (bright)
   shimmerEnv <- line (start_ (1::Float), end_ (0::Float), duration_ (0.05::Float))
   let shimmerExc = hpf (in_ (n1 ~* shimmerEnv ~* (0.02::Float)), freq_ (8000::Float))
   
   -- High resonance klank focusing on rideFreqs
   let klankSpecs = [ (f, (1.0/100.0)::Float, 1.2::Float) | f <- rideFreqs ]
   res <- klank (in_ (stick ~+ shimmerExc)) klankSpecs
   
   -- Final mix with extra high-pass to ensure clarity
   let sig = hpf (in_ res, freq_ (2000::Float)) ~+ (stick ~* (0.5::Float))
   
   s <- pan2 (in_ sig, pos_ (p ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | vividCym: "Bwoosh" approach
vividCymbalGraph = sdNamed "vividCym" (0::I "out", 5::I "sustain", 0::I "pan") $ do
    let dur = V::V "sustain"
        p = V::V "pan"
    cEnv <- line (start_ (20010::Float), end_ (10::Float), duration_ dur)
    noise <- whiteNoise
    let lodriver = lpf (in_ (noise ~* (0.1::Float)), freq_ cEnv)
    let klankSpecs = [ (f, (1.0/100.0)::Float, 1.0::Float) | f <- expFreqs ]
    res <- klank (in_ lodriver) klankSpecs
    s <- pan2 (in_ res, pos_ (p ~* (2::Float) ~- (1::Float)))
    out (V::V "out") s

-- | vividCymS: "Add a stick" approach
vividCymbalWithStick = sdNamed "vividCymS" (0::I "out", 5::I "sustain", 0::I "pan") $ do
   let dur = V::V "sustain"
       p = V::V "pan"
   locutoffenv <- line (start_ (20010::Float), end_ (10::Float), duration_ dur)
   hicEnv <- line (start_ (1000::Float), end_ (10001::Float), duration_ (1.0::Float))
   hiamplenv <- line (start_ (0.25::Float), end_ (0::Float), duration_ (dur ~* (0.6::Float)))
   thwackEnv <- line (start_ (1::Float), end_ (0::Float), duration_ (0.002::Float))
   n1 <- whiteNoise
   n2 <- whiteNoise
   let lodriver = lpf (in_ (n1 ~* (0.1::Float)), freq_ locutoffenv)
       hidriver = hpf (in_ (n2 ~* (0.1::Float)), freq_ hicEnv) ~* hiamplenv
       thwack = n1 ~* thwackEnv
   let klankSpecs = [ (f, (1.0/100.0)::Float, 1.0::Float) | f <- expFreqs ]
   res <- klank (in_ (lodriver ~+ hidriver ~+ thwack)) klankSpecs
   let sig = (res ~* (1.0::Float)) ~+ (lodriver ~* (2.0::Float)) ~+ thwack
   s <- pan2 (in_ sig, pos_ (p ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | vividCymSusp: Suspended Cymbal (soft mallets)
-- Characteristics: Slower attack, long decay, focus on resonance.
vividSuspendedCym = sdNamed "vividCymSusp" (0::I "out", 5::I "sustain", 0::I "pan") $ do
   let dur = V::V "sustain"
       p = V::V "pan"
       -- Slower attack for the "mallet" feel
       atk = dur ~* (0.2::Float)
   
   -- Excitation envelope: slow rise and fall
   -- We trigger it once with a 0Hz impulse
   excEnv <- decay2 (in_ (impulse (freq_ (0::Float))), attackSecs_ atk, decaySecs_ (dur ~* (0.8::Float)))
   
   -- Noisy excitation (gentle)
   n1 <- whiteNoise
   let excitation = lpf (in_ (n1 ~* excEnv ~* (0.1::Float)), freq_ (3000::Float))
   
   -- High resonance klank
   let klankSpecs = [ (f, (1.0/100.0)::Float, (2.0::Float)) | f <- expFreqs ]
   res <- klank (in_ excitation) klankSpecs
   
   s <- pan2 (in_ res, pos_ (p ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | vividCymSplash: Orchestral Splash/Clash Cymbal
-- Characteristics: Violent attack, broad spectrum, chaotic.
vividSplashCym = sdNamed "vividCymSplash" (0::I "out", 5::I "sustain", 0::I "pan") $ do
   let dur = V::V "sustain"
       p = V::V "pan"
   
   -- Explosive initial thwack
   thwackEnv <- line (start_ (1.5::Float), end_ (0::Float), duration_ (0.05::Float))
   
   -- Fast but rich excitation
   locEnv <- line (start_ (20000::Float), end_ (200::Float), duration_ dur)
   n1 <- whiteNoise
   n2 <- pinkNoise
   let lodriver = lpf (in_ (n1 ~* (0.2::Float)), freq_ locEnv)
       hidriver = hpf (in_ (n2 ~* (0.2::Float)), freq_ (5000::Float)) ~* (line (start_ (1::Float), end_ (0::Float), duration_ (0.5::Float)))
   
   -- Resonators
   let klankSpecs = [ (f, (1.0/100.0)::Float, 0.8::Float) | f <- expFreqs ]
   res <- klank (in_ (lodriver ~+ hidriver ~+ (n1 ~* thwackEnv))) klankSpecs
   
   let sig = (res ~* (1.2::Float)) ~+ (n1 ~* thwackEnv)
   s <- pan2 (in_ sig, pos_ (p ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s
