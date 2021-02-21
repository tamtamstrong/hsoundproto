{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IORef
import Control.Monad
import Control.Concurrent
import Data.Int (Int16, Int32)
import SDL
import qualified Data.Vector.Storable.Mutable as V

sinSamples :: [Int16]
sinSamples =
  map (\n ->
         let t = fromIntegral n / 48000 :: Double
             freq = 440
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (2 * pi * freq * t)))
      [0 :: Int32 ..]

audioCB :: IORef [Int16] -> AudioFormat sampleType -> V.IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do samples' <- readIORef samples
         let n = V.length buffer
         zipWithM_ (V.write buffer)
                   [0 ..]
                   (take n samples')
         writeIORef samples
                    (drop n samples')
    _ -> error "Unsupported audio format"

playSound :: IO ()
playSound = do 
  samples <- newIORef sinSamples
  (device,_) <-
    openAudioDevice
      OpenDeviceSpec {
        SDL.openDeviceFreq      = Mandate 48000
       ,SDL.openDeviceFormat    = Mandate Signed16BitNativeAudio
       ,SDL.openDeviceChannels  = Mandate Mono
       ,SDL.openDeviceSamples   = 4096 * 2
       ,SDL.openDeviceCallback  = audioCB samples
       ,SDL.openDeviceUsage     = ForPlayback
       ,SDL.openDeviceName      = Nothing
      }
  setAudioDevicePlaybackState device Play
  threadDelay 1000000
  closeAudioDevice device

main :: IO ()
main = do 
  initializeAll
  window <- createWindow "Sound player" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window
  quit

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  playSound
  unless qPressed (appLoop renderer)
