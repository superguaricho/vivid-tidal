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
    vividReload
    putStrLn "¡Platillos Vivid listos para Tidal!"

-- | Frecuencias exponenciales distribuidas (100 resonadores)
expFreqs :: [Float]
expFreqs = [300 * (20000/300)**(i/99) | i <- [0..99]]

-- | Primera aproximación: "Bwoosh" con resonadores en Vivid
-- Simula la transferencia de energía de graves a agudos.
vividCymbalGraph = sdNamed "vividCym" (0::I "out", 5::I "sustain", 0::I "pan") $ do
    let dur = V::V "sustain"
        p = V::V "pan"
    
    -- Envolvente para el filtro (el "bwoosh")
    -- Sube instantáneamente y cae en 'dur' segundos.
    cEnv <- line (start_ (20010::Float), end_ (10::Float), duration_ dur)
    
    -- Driver de ruido
    noise <- whiteNoise
    let lodriver = lpf (in_ (noise ~* (0.1::Float)), freq_ cEnv)
    
    -- Banco de 100 resonadores usando Klank
    -- Klank.ar(spec, input, decayscale)
    let klankSpecs = [ (f, (1.0/100.0)::Float, 1.0::Float) | f <- expFreqs ]
    res <- klank (in_ lodriver) klankSpecs

    -- Salida estéreo compatible con SuperDirt
    s <- pan2 (in_ res, pos_ ((V::V "pan") ~* (2::Float) ~- (1::Float)))
    out (V::V "out") s

-- | Versión final: "Add a stick" en Vivid
-- Incluye impacto inicial (thwack) y shimmer de alta frecuencia.
vividCymbalWithStick = sdNamed "vividCymS" (0::I "out", 5::I "sustain", 0::I "pan") $ do
   let dur = V::V "sustain"
       p = V::V "pan"

   -- Envolventes
   locutoffenv <- line (start_ (20010::Float), end_ (10::Float), duration_ dur)

   -- Shimmer: El filtro HPF sube su frecuencia de corte
   hicEnv <- line (start_ (1000::Float), end_ (10001::Float), duration_ (1.0::Float))
   hiamplenv <- line (start_ (0.25::Float), end_ (0::Float), duration_ (dur ~* (0.6::Float)))

   -- Impacto: Impulso muy corto
   thwackEnv <- line (start_ (1::Float), end_ (0::Float), duration_ (0.002::Float))

   -- Drivers de excitación
   n1 <- whiteNoise
   n2 <- whiteNoise
   let lodriver = lpf (in_ (n1 ~* (0.1::Float)), freq_ locutoffenv)
       hidriver = hpf (in_ (n2 ~* (0.1::Float)), freq_ hicEnv) ~* hiamplenv
       thwack = n1 ~* thwackEnv

   -- Resonadores excitados por la suma de drivers e impacto
   let klankSpecs = [ (f, (1.0/100.0)::Float, 1.0::Float) | f <- expFreqs ]
   res <- klank (in_ (lodriver ~+ hidriver ~+ thwack)) klankSpecs

   -- Mezcla final: Resonadores + Ruido directo + Impacto
   let sig = (res ~* (1.0::Float)) ~+ (lodriver ~* (2.0::Float)) ~+ thwack
   
   s <- pan2 (in_ sig, pos_ (p ~* (2::Float) ~- (1::Float)))
   out (V::V "out") s
