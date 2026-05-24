:set -fno-warn-orphans -Wno-type-defaults -XMultiParamTypeClasses -XOverloadedStrings
:set prompt ""
:set -i./src

-- Project modules
import Sound.Tidal.Boot
import Vivid as V
import Vivid.Tidal
import Vivid.Tidal.Examples
import Vivid.Tidal.Cymbals

default (Rational, Integer, Double, Pattern String)

-- Tidal Initialization
tidalInst <- mkTidal
instance Tidally where tidal = tidalInst

-- Load all SynthDefs and register them in SuperDirt
loadAllExamples
loadCymbals

-- Custom Tidal Parameters for Vivid Synths
:{
let grainDensity = pF "grainDensity"
    grainDur = pF "grainDur"
    freqVar = pF "freqVar"
    cutoff = pF "cutoff"
    rq = pF "rq"
    modIndex = pF "modIndex"
    modRatio = pF "modRatio"
    revMix = pF "revMix"
    revRoom = pF "revRoom"
    pos = pF "pos"
    glideTime = pF "glideTime"
    noiseType = pF "noiseType"
    beatsPerSec = pF "beatsPerSec"
    decayScale = pF "decayScale"
:}

:{
vividTidalSplash = unlines $ [
  "____   ____.__     .__    .___",
  "\\   \\ /   /|__|_  _|__| __| _/",
  " \\   Y   / | \\  \\/ /  |/ __ | ",
  "  \\     /  |  \\   /|  / /_/ | ",
  "   \\___/   |__|\\_/ |__\\____ | ",
  " ",
  "Sound synthesis with SuperCollider.",
  "Unified Boot Environment (Examples + Cymbals)",
  " "]
:}

:set prompt "tidal> "
:set prompt-cont "λ| "
    
putStrLn vividTidalSplash
putStrLn "vivid-tidal ready! Try: d1 $ s \"vividCymS*4\" # sustain 0.5"
