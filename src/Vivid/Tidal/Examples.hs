{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Vivid.Tidal.Examples where

import Vivid
import Vivid.Tidal

-- | A subtractive bass synth with a resonant low-pass filter
-- Parameters: freq, sustain, pan, cutoff, rq (resonance)
vividBass = sdNamed "vividBass" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 800::I "cutoff", 0.5::I "rq") $ do
   env <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
   
   -- Oscillator: A rich saw wave
   sig <- saw (freq_ (V::V "freq"))
   
   -- Filter: Resonant Low Pass
   filt <- rlpf (in_ sig, freq_ (V::V "cutoff"), rq_ (V::V "rq"))
   
   -- Output with SuperDirt panning logic
   s <- pan2 (in_ (filt ~* env), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | A simple 2-operator FM synth
-- Parameters: freq, sustain, pan, modIndex, modRatio
vividFM = sdNamed "vividFM" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 10::I "modIndex", 2::I "modRatio") $ do
   env <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
   
   -- Modulator
   modFreq <- (V::V "freq") ~* (V::V "modRatio")
   modSig <- sinOsc (freq_ modFreq) ~* (V::V "modIndex") ~* (V::V "freq")
   
   -- Carrier
   carSig <- sinOsc (freq_ ((V::V "freq") ~+ modSig))
   
   -- Output
   s <- pan2 (in_ (carSig ~* env), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | A percussive noise synth for hats or textures
-- Parameters: sustain, pan, cutoff
vividHats = sdNamed "vividHats" (0::I "out", 0.1::I "sustain", 0::I "pan", 10000::I "cutoff") $ do
   env <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
   
   -- Noise source
   white <- whiteNoise
   
   -- High-pass filtering to get that 'hat' sizzle
   filt <- hpf (in_ white, freq_ (V::V "cutoff"))
   
   -- Output
   s <- pan2 (in_ (filt ~* env), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | A synth that demonstrates pitch glides using Tidal's 'accelerate'
-- Parameters: freq, sustain, pan, accelerate
vividGlide = sdNamed "vividGlide" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 0::I "accelerate") $ do
   let sustain =  (V::V "sustain")
       freq = (V::V "freq")
       accel = (V::V "accelerate")
   
   ratio <- line (start_ (1::Float), end_ ((1::Float) ~+ accel), duration_ sustain)
   let glidedFreq = freq ~* ratio
   
   env <- line (start_ (1::Float), end_ (0::Float), duration_ sustain, doneAction_ (2::Float))
   sig <- saw (freq_ glidedFreq)
   
   s <- pan2 (in_ (sig ~* env), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | A granular cloud synthesizer
-- Parameters: freq, grainDensity, grainDur, freqVar, sustain, pan
vividCloud = sdNamed "vividCloud" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 20::I "grainDensity", 0.1::I "grainDur", 100::I "freqVar") $ do
   let sustain = (V::V "sustain")
       panBase = (V::V "pan")
   
   -- Main envelope for the entire note
   gateEnv <- line (start_ (1::Float), end_ (0::Float), duration_ sustain, doneAction_ (2::Float))
   
   -- Grain triggers
   trig <- dust (density_ (V::V "grainDensity"))
   
   -- Randomized frequency per grain
   noise <- whiteNoise
   randVal <- latch (in_ noise, trigger_ trig)
   let grainFreq = (V::V "freq") ~+ (randVal ~* (V::V "freqVar"))
   
   -- Grain envelope
   grainEnv <- decay2 (in_ trig, attackSecs_ (0.01::Float), decaySecs_ (V::V "grainDur"))
   
   -- Grain oscillator
   sig <- sinOsc (freq_ grainFreq) ~* grainEnv
   
   -- Output with stereo spread
   -- We mix the base pan with some per-grain randomness
   let grainPan = (panBase ~* (2::Float) ~- (1::Float) ~+ (randVal ~* (0.5::Float)))
   s <- pan2 (in_ (sig ~* gateEnv), pos_ grainPan)
   out (V::V "out") s

-- | A granular cloud with built-in reverberation
vividCloudRev = sdNamed "vividCloudRev" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 20::I "grainDensity", 0.1::I "grainDur", 100::I "freqVar", 0.5::I "revMix", 0.5::I "revRoom") $ do
   let sustain = (V::V "sustain")
       panBase = (V::V "pan")
   
   gateEnv <- line (start_ (1::Float), end_ (0::Float), duration_ sustain, doneAction_ (2::Float))
   trig <- dust (density_ (V::V "grainDensity"))
   
   noise <- whiteNoise
   randVal <- latch (in_ noise, trigger_ trig)
   let grainFreq = (V::V "freq") ~+ (randVal ~* (V::V "freqVar"))
   grainEnv <- decay2 (in_ trig, attackSecs_ (0.01::Float), decaySecs_ (V::V "grainDur"))
   
   sig <- sinOsc (freq_ grainFreq) ~* grainEnv
   
   let grainPan = (panBase ~* (2::Float) ~- (1::Float) ~+ (randVal ~* (0.5::Float)))
   s <- pan2 (in_ (sig ~* gateEnv), pos_ grainPan)
   
   let left = s !! 0
       right = s !! 1
   
   wetL <- freeVerb (in_ left, mix_ (V::V "revMix"), room_ (V::V "revRoom"), damp_ (0.5::Float))
   wetR <- freeVerb (in_ right, mix_ (V::V "revMix"), room_ (V::V "revRoom"), damp_ (0.5::Float))
   
   out (V::V "out") [wetL, wetR]

-- | A granular sampler that uses SuperDirt buffers
vividSampler = sdNamed "vividSampler" (0::I "out", 1::I "sustain", 0::I "pan", 0::I "bufnum", 20::I "grainDensity", 0.1::I "grainDur", 0::I "begin", 1::I "end", 1::I "speed") $ do
   let sustain = (V::V "sustain")
       buf = (V::V "bufnum")
       speed = (V::V "speed")
   
   gateEnv <- line (start_ (1::Float), end_ (0::Float), duration_ sustain, doneAction_ (2::Float))
   trig <- dust (density_ (V::V "grainDensity"))
   
   frames <- bufFrames buf
   let startFrame = frames ~* (V::V "begin")
       endFrame = frames ~* (V::V "end")
       range = endFrame ~- startFrame
   
   noise <- whiteNoise
   posRand <- latch (in_ (noise ~* (0.5::Float) ~+ (0.5::Float)), trigger_ trig)
   let grainStart = startFrame ~+ (posRand ~* range)
   
   grainEnv <- decay2 (in_ trig, attackSecs_ (0.01::Float), decaySecs_ (V::V "grainDur"))
   
   sigs <- playBufPoly 1 (buf_ buf, rate_ (speed ~* bufRateScale buf), trigger_ trig, startPos_ grainStart, loop_ (1::Float))
   let sig = sigs !! 0
   
   s <- pan2 (in_ (sig ~* grainEnv ~* gateEnv), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | A time-stretching granular warp synth
vividWarp = sdNamed "vividWarp" (0::I "out", 1::I "sustain", 0::I "pan", 0::I "bufnum", 1::I "speed", 0::I "pos") $ do
   let sustain = (V::V "sustain")
       buf = (V::V "bufnum")
       speed = (V::V "speed")
   
   gateEnv <- line (start_ (1::Float), end_ (0::Float), duration_ sustain, doneAction_ (2::Float))
   trig <- dust (density_ (100::Float))
   frames <- bufFrames buf
   
   let rate = bufRateScale buf
   phase <- phasor (trig_ (0::Float), rate_ (rate ~* (V::V "pos")), start_ (0::Float), end_ frames)
   
   grainStart <- latch (in_ phase, trigger_ trig)
   grainEnv <- decay2 (in_ trig, attackSecs_ (0.01::Float), decaySecs_ (0.1::Float))
   
   sigs <- playBufPoly 1 (buf_ buf, rate_ speed, trigger_ trig, startPos_ grainStart, loop_ (1::Float))
   let sig = sigs !! 0
   
   s <- pan2 (in_ (sig ~* grainEnv ~* gateEnv), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | An advanced slide synth using exponential ramps
vividSlide = sdNamed "vividSlide" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 0::I "accelerate", 0.1::I "glideTime") $ do
  let freq = V::V "freq"
      accel = V::V "accelerate"
  targetFreq <- xLine (start_ freq, end_ (freq ~* ((1::Float) ~+ accel)), duration_ (V::V "glideTime"))
  env <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
  sig <- saw (freq_ targetFreq)
  s <- pan2 (in_ (sig ~* env), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
  out (V::V "out") s

(~==) = binaryOp Eq
(~>=) = binaryOp Ge

-- | A noise synth with a resonant band-pass filter and noise selection
vividNoise = sdNamed "vividNoise" (0::I "out", 1::I "sustain", 0::I "pan", 1000::I "cutoff", 0.1::I "rq", 0::I "noiseType") $ do
  let nType = V::V "noiseType"
  env <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
  w <- whiteNoise ; p <- pinkNoise ; b <- brownNoise
  let sig = (w ~* (nType ~== (0::Float))) ~+ (p ~* (nType ~== (1::Float))) ~+ (b ~* (nType ~>= (2::Float)))
  filt <- bpf (in_ sig, freq_ (V::V "cutoff"), rq_ (V::V "rq"))
  s <- pan2 (in_ (filt ~* env), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
  out (V::V "out") s

-- | A synchronous rhythmic granular synth
vividGrainBeat = sdNamed "vividGrainBeat" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 0.1::I "grainDur", 8::I "beatsPerSec") $ do
   let bps = V::V "beatsPerSec"
   gateEnv <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
   
   -- Using LFPulse as a gate is more sound for rhythmic pulses
   let pulseWidth = (V::V "grainDur") ~* bps
   m <- lfPulse (freq_ bps, width_ pulseWidth)
   
   sig <- saw (freq_ (V::V "freq"))
   s <- pan2 (in_ (sig ~* m ~* gateEnv), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | An advanced granular synth with pitch and size jitter
vividGrains = sdNamed "vividGrains" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 20::I "grainDensity", 0.1::I "grainDur", 50::I "freqVar") $ do
   gateEnv <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
   trig <- dust (density_ (V::V "grainDensity"))
   noise1 <- whiteNoise ; noise2 <- pinkNoise
   randFreq <- latch (in_ noise1, trigger_ trig)
   let grainFreq = (V::V "freq") ~+ (randFreq ~* (V::V "freqVar"))
   randDur <- latch (in_ noise2, trigger_ trig)
   let actualDur = (V::V "grainDur") ~* (randDur ~* (0.5::Float) ~+ (1.0::Float))
   grainEnv <- decay2 (in_ trig, attackSecs_ (0.01::Float), decaySecs_ actualDur)
   sig <- sinOsc (freq_ grainFreq) ~* grainEnv
   let grainPan = ((V::V "pan") ~* (2::Float) ~- (1::Float)) ~+ (randFreq ~* (0.4::Float))
   s <- pan2 (in_ (sig ~* gateEnv), pos_ grainPan)
   out (V::V "out") s

-- | Granular cloud with per-grain filtering
vividGrainFilter = sdNamed "vividGrainFilter" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 20::I "grainDensity", 0.1::I "grainDur", 2000::I "cutoff", 0.1::I "rq") $ do
   gateEnv <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
   trig <- dust (density_ (V::V "grainDensity"))
   noise <- whiteNoise
   randCutoff <- latch (in_ noise, trigger_ trig)
   let grainCutoff = (V::V "cutoff") ~+ (randCutoff ~* (1000::Float))
   grainEnv <- decay2 (in_ trig, attackSecs_ (0.01::Float), decaySecs_ (V::V "grainDur"))
   sig <- saw (freq_ (V::V "freq")) ~* grainEnv
   filt <- bpf (in_ sig, freq_ grainCutoff, rq_ (V::V "rq"))
   s <- pan2 (in_ (filt ~* gateEnv), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | A metallic resonator bank excited by noise
-- Parameters: freq, sustain, pan, decayScale
vividMetallic = sdNamed "vividMetallic" (0::I "out", 1::I "sustain", 0::I "pan", 440::I "freq", 1::I "decayScale") $ do
   let freq = V::V "freq"
       dScale = V::V "decayScale"
   
   env <- line (start_ (1::Float), end_ (0::Float), duration_ (V::V "sustain"), doneAction_ (2::Float))
   
   -- Excitation: very short burst of noise
   excitation <- pinkNoise ~* line (start_ (0.1::Float), end_ (0::Float), duration_ (0.01::Float))
   
   -- Resonators: A list of (frequency, amplitude, ringTime)
   let resonators = [ (freq ~* (1.0::Float), 1.0::Float, 1.0::Float)
                    , (freq ~* (1.5::Float), 0.5::Float, 0.8::Float)
                    , (freq ~* (2.1::Float), 0.3::Float, 0.6::Float)
                    , (freq ~* (3.7::Float), 0.2::Float, 0.4::Float)
                    , (freq ~* (5.2::Float), 0.1::Float, 0.3::Float) ]
   
   sig <- klank (in_ excitation, decayScale_ dScale) resonators
   
   s <- pan2 (in_ (sig ~* env), pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s

-- | Helper to load all examples at once
loadAllExamples :: IO ()
loadAllExamples = do
   putStrLn "Pushing all Vivid examples into SuperCollider (Server + Disk)..."
   vividPush "vividBass" vividBass
   vividPush "vividFM" vividFM
   vividPush "vividHats" vividHats
   vividPush "vividGlide" vividGlide
   vividPush "vividCloud" vividCloud
   vividPush "vividCloudRev" vividCloudRev
   vividPush "vividSampler" vividSampler
   vividPush "vividWarp" vividWarp
   vividPush "vividSlide" vividSlide
   vividNoisePush
   vividPush "vividGrainBeat" vividGrainBeat
   vividPush "vividGrains" vividGrains
   vividPush "vividGrainFilter" vividGrainFilter
   vividPush "vividMetallic" vividMetallic
   vividPush "vividProxy" vividProxy
   putStrLn "Notifying SuperDirt..."
   vividReload
   putStrLn "All examples pushed!"
 where
   vividNoisePush = vividPush "vividNoise" vividNoise
